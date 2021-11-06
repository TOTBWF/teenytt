{
module TeenyTT.Frontend.Parser.Lexer where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

import TeenyTT.Frontend.Parser.Lexer.Monad
import TeenyTT.Frontend.Parser.Token


}

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z09]

tokens :-

-- We don't want to include newlines as part of our whitespace.
[\ \t]+ ;

<0> "--" .* \n { \_ -> pushStartCode newline *> scan }
<0> \n         { \_ -> pushStartCode newline *> scan }

<0> (λ|\\)                        { token_ Lambda }
<0> :                             { token_ Colon }
<0> =                             { token_ Equal }
<0> (→|\->)                       { token_ Arrow }
<0> (∀|\forall)                   { token_ ForAll }
<0> \(                            { token_ LParen }
<0> \)                            { token_ RParen }
<0> $alpha [$alpha $digit \_ \-]* { token Identifier }

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
handleEOF :: Lexer Token
handleEOF = pushStartCode eof *> scan

-- | Closes out any layout blocks if they exist, and then emits an 'EOF' token.
emitEOF :: ByteString -> Lexer Token
emitEOF _ = do
  block <- currentBlock
  case block of
    Just _ -> do
      closeBlock
      pure BlockClose
    Nothing -> do
      popStartCode
      pure EOF

startLayout :: ByteString -> Lexer Token
startLayout _ = do
  popStartCode
  block <- currentBlock
  col <- getColumn
  -- If are inside of some layout block, /and/
  -- the column of the next token is to the left (or equal to)
  -- the indentation of that block, we enter the 'empty_layout' state
  -- so that we can close out the block.
  -- If this is not the case, we can push a new layout block to the stack.
  if (Just col) <= block
    then pushStartCode empty_layout
    else openBlock col

  pure BlockOpen

emptyLayout :: ByteString -> Lexer Token
emptyLayout _ = do
  popStartCode
  pushStartCode newline
  pure BlockClose

-- | The offsides rule gets invoked every time we encounter
--   a newline, and determines if we ought to continue with
--   our current layout block or not based off of the indentation
--   of the first token we encounter.
offsides :: ByteString -> Lexer Token
offsides _ = do
  block <- currentBlock
  col <- getColumn
  case block of
    Just layoutCol ->
      case col `compare` layoutCol of
        EQ -> do
          -- The current column is the same as the
          -- layout column, we exit out of the 'newline'
          -- state and then also emit a token denoting
          -- that there was a linebreak within the block.
          popStartCode
          pure BlockBreak
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
          pure BlockClose
    Nothing -> do
      -- If we aren't currently in a layout context,
      -- exit out of the 'newline' state and keep scanning.
      popStartCode
      scan

scan :: Lexer Token
scan = do
    input <- getInput
    code <- startCode
    case alexScan input code of
      AlexEOF -> handleEOF
      AlexError rest -> lexError rest
      AlexSkip rest len -> do
        setInput rest
        scan
      AlexToken rest nbytes action -> do
        setInput rest
        action (slice nbytes input)

lexer :: Lexer [Token]
lexer = do
    tok <- scan
    case tok of
      EOF -> pure []
      x -> (x :) <$> lexer
}
