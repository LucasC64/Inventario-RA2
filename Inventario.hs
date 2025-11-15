{-# LANGUAGE DeriveGeneric #-}

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Time (UTCTime, getCurrentTime)
import System.IO (hFlush, stdout)
import System.Directory (doesFileExist)
import Control.Exception (catch, IOException)
import Data.List (foldl', group, sort, maximumBy)
import Data.Ord (comparing)
import Data.Char (isSpace)
import GHC.Generics (Generic)
import Prelude hiding (readFile, writeFile)
import qualified Prelude (readFile, writeFile)

data Item = Item
  { itemID    :: String
  , nome      :: String
  , quantidade:: Int
  , categoria :: String
  } deriving (Show, Read, Eq, Generic)

data StatusLog = Sucesso | Falha String
  deriving (Show, Read, Eq, Generic)

data AcaoLog
  = Add String String Int String
  | Remove String Int
  | Update String Int
  | QueryFail String
  deriving (Show, Read, Eq, Generic)

data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao      :: AcaoLog
  , detalhes  :: String
  , status    :: StatusLog
  } deriving (Show, Read, Eq, Generic)

type Inventario = Map String Item
type ResultadoOperacao = (Inventario, LogEntry)

arquivoInv, arquivoLog :: FilePath
arquivoInv = "Inventario.dat"
arquivoLog = "Auditoria.log"

validateNonNeg :: Int -> Either String Int
validateNonNeg n = if n < 0 then Left "quantidade negativa" else Right n

validatePos :: Int -> Either String Int
validatePos n = if n <= 0 then Left "quantidade deve ser maior que zero" else Right n

applyAction :: AcaoLog -> Inventario -> Inventario
applyAction (Add k nm q cat) inv =
  case M.lookup k inv of
    Just it -> M.insert k (it { quantidade = quantidade it + q }) inv
    Nothing -> M.insert k (Item k nm q cat) inv
applyAction (Remove k q) inv =
  case M.lookup k inv of
    Nothing -> inv
    Just it ->
      let novo = quantidade it - q
      in if novo <= 0 then M.delete k inv else M.insert k (it { quantidade = novo }) inv
applyAction (Update k q) inv =
  case M.lookup k inv of
    Nothing -> M.insert k (Item k "" q "") inv
    Just it -> M.insert k (it { quantidade = q }) inv
applyAction (QueryFail _) inv = inv

findByName :: String -> Inventario -> Maybe Item
findByName nm inv = case filter (\it -> nome it == nm) (M.elems inv) of
  (x:_) -> Just x
  [] -> Nothing

generateUnique :: String -> Inventario -> String
generateUnique base inv =
  head $ filter (`M.notMember` inv) [base ++ "_" ++ show n | n <- [1 :: Int ..]]

prefixResolve :: String -> Inventario -> Either String String
prefixResolve pref inv =
  let ks = M.keys inv
      found = filter (pref `isPrefixOf`) ks
  in case found of
       [] -> Left ("id nao encontrado: " ++ pref)
       [x] -> Right x
       _ -> Left ("id ambíguo: " ++ pref)
  where
    isPrefixOf p s = take (length p) s == p

addItem :: UTCTime -> String -> String -> Int -> String -> Inventario -> Either String ResultadoOperacao
addItem t pedido nomeItem q cat inv = do
  _ <- validatePos q
  let parecido a b = take 3 a == take 3 b
  case filter (\it -> parecido (nome it) nomeItem) (M.elems inv) of
    (it:_) ->
      let alvo = itemID it
          ac = Add alvo nomeItem q (categoria it)
          lg = LogEntry t ac ("agregado a: " ++ alvo) Sucesso
      in Right (applyAction ac inv, lg)
    [] ->
      let usado = if M.member pedido inv then generateUnique pedido inv else pedido
          ac = Add usado nomeItem q cat
          lg = LogEntry t ac ("criado id: " ++ usado) Sucesso
      in Right (applyAction ac inv, lg)

removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem t maybeId q inv = do
  _ <- validatePos q
  resolved <- if M.member maybeId inv then Right maybeId else prefixResolve maybeId inv
  case M.lookup resolved inv of
    Nothing -> Left "item ausente apos resolver"
    Just it -> if quantidade it < q then Left "estoque insuficiente" else do
      let ac = Remove resolved q
      let lg = LogEntry t ac ("removido: " ++ resolved) Sucesso
      Right (applyAction ac inv, lg)

updateItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateItem t maybeId q inv = do
  _ <- validateNonNeg q
  resolved <- if M.member maybeId inv then Right maybeId else prefixResolve maybeId inv
  let ac = Update resolved q
  let lg = LogEntry t ac ("atualizado: " ++ resolved) Sucesso
  Right (applyAction ac inv, lg)

reconstruct :: [LogEntry] -> Inventario
reconstruct = foldl' (\acc (LogEntry _ ac _ st) -> case st of { Sucesso -> applyAction ac acc; _ -> acc }) M.empty

idsFromLog :: LogEntry -> [String]
idsFromLog (LogEntry _ ac _ _) = case ac of
  Add k _ _ _ -> [k]
  Remove k _ -> [k]
  Update k _ -> [k]
  QueryFail _ -> []

groupCount :: Ord a => [a] -> [(a, Int)]
groupCount xs = map (\g -> (head g, length g)) . group . sort $ xs

historicoPorItem :: [LogEntry] -> [(String, Int)]
historicoPorItem logs = let ids = concatMap idsFromLog logs; cnt = groupCount ids in map (\(k,n) -> (k,n)) cnt

logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter (\(LogEntry _ _ _ st) -> case st of { Falha _ -> True; _ -> False })

itemMaisMovimentado :: [LogEntry] -> Maybe (String, Int)
itemMaisMovimentado logs =
  let ids = concatMap idsFromLog logs
  in if null ids then Nothing else Just (maximumBy (comparing snd) (groupCount ids))

loadCache :: IO Inventario
loadCache = do
  ex <- doesFileExist arquivoInv
  if not ex then return M.empty else catch (do
    txt <- Prelude.readFile arquivoInv
    case reads txt of
      [(v,"")] -> return v
      _ -> return M.empty) (\(_::IOException) -> return M.empty)

loadLogs :: IO [LogEntry]
loadLogs = do
  ex <- doesFileExist arquivoLog
  if not ex then return [] else catch (do
    txt <- Prelude.readFile arquivoLog
    return $ foldl' (\acc ln -> case reads ln of { [(v,"")] -> v:acc; _ -> acc }) [] (lines txt)) (\(_::IOException) -> return [])

saveCache :: Inventario -> IO ()
saveCache inv = Prelude.writeFile arquivoInv (show inv)

appendLog :: LogEntry -> IO ()
appendLog e = Prelude.appendFile arquivoLog (show e ++ "\n")

makeFailLog :: UTCTime -> AcaoLog -> String -> LogEntry
makeFailLog t a m = LogEntry t a m (Falha m)

parseArgs :: String -> [String]
parseArgs s = reverse (go [] (dropWhile isSpace s))
  where
    go acc [] = acc
    go acc ('"':xs) =
      let (w,r) = span (/= '"') xs
      in go (w:acc) (dropWhile isSpace (drop 1 r))
    go acc xs =
      let (w,r) = break isSpace xs
      in go (w:acc) (dropWhile isSpace r)

execResult :: UTCTime -> Either String ResultadoOperacao -> Inventario -> IO Inventario
execResult now (Right (inv2, lg)) _ = do
  appendLog lg
  saveCache inv2
  putStrLn "ok"
  return inv2
execResult now (Left err) inv = do
  let lg = makeFailLog now (QueryFail err) err
  appendLog lg
  putStrLn ("erro: " ++ err)
  return inv

listItems :: Inventario -> IO ()
listItems inv = mapM_ showOne (M.elems inv)
  where
    showOne it = putStrLn $ itemID it ++ " | " ++ nome it ++ " | qtd: " ++ show (quantidade it) ++ " | cat: " ++ categoria it

showOne :: String -> Inventario -> IO ()
showOne s inv = case M.lookup s inv of
  Nothing -> putStrLn "nao achei"
  Just it -> print it

seed :: IO Inventario
seed = do
  now <- getCurrentTime
  logs <- loadLogs
  let current = reconstruct logs
  if M.size current >= 10 then return current else do
    let needed = [ "item" ++ show n | n <- [1..10], M.notMember ("item" ++ show n) current ]
    let evs' = zipWith (\i n -> Add i ("produto" ++ show n) (n*3) ("cat" ++ show ((n `mod` 3)+1))) needed [1..length needed]
    let entries = [ LogEntry now e "seed" Sucesso | e <- evs' ]
    mapM_ appendLog entries
    let inv' = reconstruct (entries ++ logs)
    saveCache inv'
    return inv'

report :: IO ()
report = do
  logs <- loadLogs
  putStrLn "historico por item:"
  mapM_ (\(k,n) -> putStrLn $ k ++ ": " ++ show n) (historicoPorItem logs)
  putStrLn "logs de erro:"
  mapM_ print (logsDeErro logs)
  putStrLn "item mais movimentado:"
  case itemMaisMovimentado logs of
    Nothing -> putStrLn "nenhum item registrado"
    Just (k,n) -> putStrLn $ k ++ " (" ++ show n ++ ")"

process :: [String] -> Inventario -> IO Inventario
process args inv = do
  now <- getCurrentTime
  case args of
    ("add":kid:nm:q:xs) ->
      case reads q of
        [(qq,"")] -> execResult now (addItem now kid nm qq (unwords xs) inv) inv
        _ -> putStrLn "qtd invalida" >> return inv
    ("remove":kid:q:_) ->
      case reads q of
        [(qq,"")] -> execResult now (removeItem now kid qq inv) inv
        _ -> putStrLn "qtd invalida" >> return inv
    ("update":kid:q:_) ->
      case reads q of
        [(qq,"")] -> execResult now (updateItem now kid qq inv) inv
        _ -> putStrLn "qtd invalida" >> return inv
    ("list":_) -> listItems inv >> return inv
    ("show":x:_) -> showOne x inv >> return inv
    ("seed":_) -> seed
    ("report":_) -> report >> return inv
    ("sair":_) -> putStrLn "encerrando..." >> return inv
    _ -> putStrLn "nao entendi" >> return inv

loop :: Inventario -> IO ()
loop inv = do
  putStr "inventario> "
  hFlush stdout
  l <- catch getLine (\(_::IOException) -> return "")
  let p = parseArgs l
  if null p || head p == "sair"
    then putStrLn "tchau"
    else do
      inv2 <- process p inv
      loop inv2

main :: IO ()
main = do
  logs <- loadLogs
  cache <- loadCache
  let invFromLog = reconstruct logs
  let invToUse = if M.null cache then invFromLog else cache
  loop invToUse



