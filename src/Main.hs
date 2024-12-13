import Control.Monad (void)
import Data.Maybe (catMaybes)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String


parseMul :: P.Parsec v String Int
parseMul = product <$> P.between "mul(" ")" (numberParser `P.sepBy` ",")

part1 :: Parsec v Int
part1 = sum <$> many (dropUntil parseMul)


numberParser :: Parser Int
numberParser = do
    digits <- many digitChar
    if length digits > 3
        then
            fail "Number too long"
        else
            pure $ read digits

parseMul :: Parsœer Int

validMul :: Parser (Maybe (Int, Int))
validMul = do
    _ <- string "mul("
    n1 <- numberParser
    _ <- char ','
    n2 <- numberParser
    _ <- char ')'
    return (Just (n1, n2))

-- Skip any character that isn't the start of a valid mul
skipJunk :: Parser (Maybe (Int, Int))
skipJunk = do
    () <- void $ manyTill anySingle (lookAhead (void $ string "mul(") <|> eof)
    pure Nothing

skipOne :: Parser (Maybe (Int, Int))
skipOne = do
    void anySingle
    pure Nothing

langParser :: Parser [Maybe (Int, Int)]
langParser = catMaybes <$> many (skipJunk *> optional validMul)

main :: IO ()
main = do
    contents <- readFile "./puzzles/03/test"
    putStrLn "Running"
    case parse langParser "" contents of
        Left err -> putStrLn $ errorBundlePretty err
        Right l -> print l
