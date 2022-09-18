module Parser (
  parse,
  parseData,
  JSData(..)
) where

import qualified Data.ByteString.Lazy as BL
import qualified Scanner as S
import qualified Data.Map as Map

data JSData
  = JSNull
  | JSTrue
  | JSFalse
  | JSNumber String
  | JSString String
  | JSList [JSData]
  | JSObject (Map.Map String JSData)
  deriving (Show, Eq)

parse :: BL.ByteString -> (Maybe JSData, String)
parse stream =
  case S.scan stream of
    ([], msg) ->
      (Nothing, msg)

    (tokens, scanMsg) ->
      let
        (result, ts) = parseData tokens
      in
        case result of
          Left parseMsg ->
            (Nothing, scanMsg ++ "\n" ++ parseMsg)

          Right jsData ->
            (Just jsData, scanMsg)

parseData :: [S.Token] -> (Either String JSData, [S.Token])
parseData tokens =
  case tokens of
    [] ->
      (Left "End of stream", [])

    (S.JNull:ts) ->
      (Right JSNull, ts)
    
    (S.JTrue:ts) ->
      (Right JSTrue, ts)

    (S.JFalse:ts) ->
      (Right JSFalse, ts)

    (S.JNumber str:ts) ->
      (Right (JSNumber str), ts)
    
    (S.JString str:ts) ->
      (Right (JSString str), ts)
    
    (S.JOpeningSquare:ts) ->
      case ts of
        [] ->
          (Left "End of stream", [])
        
        (t':ts') ->
          parseEmptyListHelper t' ts'

    (S.JOpeningBrace:ts) ->
      case ts of
        [] ->
          (Left "End of stream", [])
        
        (t':ts') ->
          parseEmptyObjectHelper t' ts'
    (t:ts) ->
      (Left ("Unexpected token: " ++ show t), ts)

parseEmptyListHelper :: S.Token
  -> [S.Token]
  -> (Either String JSData, [S.Token])
parseEmptyListHelper t ts =
  case t of
    S.JClosingSquare ->
      (Right (JSList []), ts)
    
    _ ->
      case parseData (t:ts) of
        (Left msg, ts') ->
          (Left msg, ts')

        (Right jsData, ts') ->
          let
            (result, ts'') = parseEndListHelper (Right [jsData], ts')
          in
            case result of
              Right ls ->
                (Right (JSList ls), ts'')

              Left msg ->
                (Left msg, ts'')

parseEndListHelper :: (Either String [JSData], [S.Token])
  -> (Either String [JSData], [S.Token])
parseEndListHelper (Left msg, ts) =
  (Left msg, ts)

parseEndListHelper (Right ls, ts) =
  case ts of
    [] ->
      (Left "End of stream", ts)

    (S.JClosingSquare:ts') ->
      (Right ls, ts')

    (S.JComma:ts') ->
      case parseData ts' of
        (Left msg, ts') ->
          (Left msg, ts')

        (Right jsData, ts') ->
          let
            (result, ts'') = parseEndListHelper (Right [jsData], ts')
          in
            case result of
              Right ls' ->
                (Right (ls ++ ls'), ts'')

              Left msg ->
                (Left msg, ts'')
    
    (t:ts') ->
      (Left ("Unexpected token: " ++ show t), ts')

parseEmptyObjectHelper :: S.Token
  -> [S.Token]
  -> (Either String JSData, [S.Token])
parseEmptyObjectHelper t ts =
  case t of
    S.JClosingBrace ->
      (Right (JSObject Map.empty), ts)
    
    _ ->
      case parsePair (t:ts) of
        (Left msg, ts') ->
          (Left msg, ts')

        (Right pair, ts') ->
          let
            (result, ts'') = parseEndObjectHelper (Right [pair], ts')
          in
            case result of
              Right ls ->
                (Right (JSObject (Map.fromList ls)), ts'')

              Left msg ->
                (Left msg, ts'')

parseEndObjectHelper :: (Either String [(String, JSData)], [S.Token])
  -> (Either String [(String, JSData)], [S.Token])
parseEndObjectHelper (Left msg, ts) =
  (Left msg, ts)

parseEndObjectHelper (Right ls, ts) =
  case ts of
    [] ->
      (Left "End of stream", ts)

    (S.JClosingBrace:ts') ->
      (Right ls, ts')

    (S.JComma:ts') ->
      case parsePair ts' of
        (Left msg, ts') ->
          (Left msg, ts')

        (Right pair, ts') ->
          let
            (result, ts'') = parseEndObjectHelper (Right [pair], ts')
          in
            case result of
              Right ls' ->
                (Right (ls ++ ls'), ts'')

              Left msg ->
                (Left msg, ts'')
    
    (t:ts') ->
      (Left ("Unexpected token: " ++ show t), ts')

parsePair :: [S.Token] -> (Either String (String, JSData), [S.Token])
parsePair tokens =
  case tokens of
    [] ->
      (Left "End of stream", [])

    (S.JString str:ts) ->
      case ts of
        [] ->
          (Left "End of stream", [])

        (S.JColon:ts') ->
          case parseData ts' of
            (Left msg, ts'') ->
              (Left msg, ts'')

            (Right jsData, ts'') ->
              (Right (str, jsData), ts'')

        (t':ts') ->
          (Left ("Expected Colon instead got " ++ show t'), ts)
    
    (t:ts) ->
      (Left ("Expected String instead got " ++ show t), ts)