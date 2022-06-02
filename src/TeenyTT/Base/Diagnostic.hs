-- | Diagnostics
module TeenyTT.Base.Diagnostic
  ( Diagnostic(..)
  , Severity(..)
  , Code(..)
  , Snippet(..)
  , render
  ) where

import Control.Exception

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)

import TeenyTT.Base.Location
import TeenyTT.Base.Pretty

--------------------------------------------------------------------------------
-- Diagnostics

data Diagnostic
    = Diagnostic
    { severity :: Severity
    , code     :: Code
    , snippets :: [Snippet]
    }
    deriving stock Show
    deriving anyclass Exception

data Severity
    = Info
    | Warning
    | Error
    | Panic
    deriving stock Show

instance Pretty Severity where
    pretty Info = "Info"
    pretty Warning = "Warning"
    pretty Error = "Error"
    pretty Panic = "Panic"

--------------------------------------------------------------------------------
-- Error Codes

data Code
    = LexError
    | ParseError
    | Impossible Text
    deriving stock Show

instance Pretty Code where
    pretty LexError   = "[E001]: Lexer Error"
    pretty ParseError = "[E002]: Parser Error"
    pretty (Impossible msg) = "[XXXX]: The Impossible happened:" <+> pretty msg

--------------------------------------------------------------------------------
-- Snippets

data Snippet
    = Snippet
    { location :: Span
    , message  :: Doc ()
    }
    deriving stock Show

--------------------------------------------------------------------------------
-- Rendering

renderSnippet :: ByteString -> Snippet -> Doc ()
renderSnippet buffer snippet =
    let sourceBytes = sliceLine snippet.location buffer
        source = pretty $ decodeUtf8 $ sourceBytes
        underline = pretty $ replicate snippet.location.width '^'
        fringeWidth = snippet.location.startLine `div` 10 + 2
    -- [FIXME: Reed M, 02/06/2022] For some reason the error message are jacked up for parse errors.
    in vcat [
              indent fringeWidth ">" <+> snippet.message
            , indent fringeWidth "│"
            , pretty snippet.location.startLine <+> "│" <+> source
            , indent fringeWidth "│" <+> indent snippet.location.startCol underline
            ]

render :: ByteString -> Diagnostic -> Doc ()
render buffer diag =
    let header = pretty diag.severity <+> (pretty diag.code)
    in vcat (header:fmap (renderSnippet buffer) diag.snippets)
