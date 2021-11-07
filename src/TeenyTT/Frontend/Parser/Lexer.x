{
module TeenyTT.Frontend.Parser.Lexer where

import Data.ByteString (ByteString)

import TeenyTT.Frontend.Parser.Monad
import TeenyTT.Frontend.Parser.Token


}

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z09]

--------------------------------------------------------------------------------
-- Macros
@natural   = $digit+
@ident     = $alpha [$alpha $digit \_ \-]*
@directive = \# @ident

tokens :-

-- We don't want to include newlines as part of our whitespace.
[\ \t]+ ;

-- [FIXME: Reed M, 06/11/2021] Figure out a better way of doing keywords
<0> "--" .* \n { \_ -> pushStartCode newline *> scan }
<0> \n         { \_ -> pushStartCode newline *> scan }

<0> (λ|\\)                        { symbol Lambda }
<0> :                             { symbol Colon }
<0> =                             { symbol Equal }
<0> _                             { symbol Underscore }
<0> (→|\->)                       { symbol Arrow }
<0> (∀|\forall)                   { symbol ForAll }
<0> \(                            { symbol LParen }
<0> \)                            { symbol RParen }
<0> \{\!                          { symbol LBang }
<0> \!\}                          { symbol RBang }
<0> \?                            { symbol Question }
<0> Type                          { keyword Type }
<0> ℕ                             { keyword Nat }
<0> suc                           { keyword Suc }
<0> @natural                      { literal NumLit }
<0> @ident                        { token TokIdent }
<0> @directive                    { token TokDirective }

--------------------------------------------------------------------------------
-- Layout
--
-- For layout, we have the following start codes:
-- * layout:
--   We have just encountered a layout keyword, and are
--   trying to determine the column for the layout block.
-- * empty_layout:
--   We enter this state when the token following a layout keyword is
--   on the same column or to the left of the current layout context.
-- * newline:
--   We enter this state at the beginning of each new line.
--   The only possible thing we can do here is check the layout
--   rule via offsides.
-- * eof:
--   As the name suggests, we enter this state once we reach the end of the file.
--   All we can do here is close out any open layout blocks, and then finally emit
--   an EOF token.

<layout> { "--" .* \n ; \n ; () { startLayout } }
<empty_layout> () { emptyLayout }
<newline> { \n ; "--" .* \n ; () { offsides } }
<eof> () { emitEOF }

{
handleEOF :: Parser Token
handleEOF = pushStartCode eof *> scan

-- | Closes out any layout blocks if they exist, and then emits an 'EOF' token.
emitEOF :: ByteString -> Parser Token
emitEOF _ = do
  block <- currentBlock
  case block of
    Just _ -> do
      closeBlock
      TokSymbol BlockClose <$> getSpan
    Nothing -> do
      popStartCode
      pure EOF

startLayout :: ByteString -> Parser Token
startLayout _ = do
  popStartCode
  block <- currentBlock
  col <- getParseColumn
  -- If are inside of some layout block, /and/
  -- the column of the next token is to the left (or equal to)
  -- the indentation of that block, we enter the 'empty_layout' state
  -- so that we can close out the block.
  -- If this is not the case, we can push a new layout block to the stack.
  if (Just col) <= block
    then pushStartCode empty_layout
    else openBlock col

  TokSymbol BlockOpen <$> getSpan

emptyLayout :: ByteString -> Parser Token
emptyLayout _ = do
  popStartCode
  pushStartCode newline
  TokSymbol BlockClose <$> getSpan

-- | The offsides rule gets invoked every time we encounter
--   a newline, and determines if we ought to continue with
--   our current layout block or not based off of the indentation
--   of the first token we encounter.
offsides :: ByteString -> Parser Token
offsides _ = do
  block <- currentBlock
  col <- getParseColumn
  case block of
    Just layoutCol ->
      case col `compare` layoutCol of
        EQ -> do
          -- The current column is the same as the
          -- layout column, we exit out of the 'newline'
          -- state and then also emit a token denoting
          -- that there was a linebreak within the block.
          popStartCode
          TokSymbol BlockBreak <$> getSpan
        GT -> do
          -- If the current column is greater than
          -- the layout column, we exit out of the 'newline'
          -- state, and don't emit anything, as the block
          -- can simply be continued.
          popStartCode
          scan
        LT -> do
          -- If the current cloumn is less than
          -- the layout column, we need to close out the block!
          closeBlock
          TokSymbol BlockClose <$> getSpan
    Nothing -> do
      -- If we aren't currently in a layout context,
      -- exit out of the 'newline' state and keep scanning.
      popStartCode
      scan

scan :: Parser Token
scan = do
    input <- getInput
    code <- startCode
    case alexScan input code of
      AlexEOF -> handleEOF
      AlexError rest -> parseError "Lexer Error"
      AlexSkip rest len -> do
        setInput rest
        scan
      AlexToken rest nbytes action -> do
        setInput rest
        action (slice nbytes input)

lexer :: Parser [Token]
lexer = do
    tok <- scan
    case tok of
      EOF -> pure []
      x -> (x :) <$> lexer
}
