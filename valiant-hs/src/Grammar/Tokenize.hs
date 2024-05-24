module Grammar.Tokenize where

tokenizers :: [(String, String -> [String])]
tokenizers =
  [ ("words", words),
    ("lines", lines),
    ("chars", foldr ((:) . (: [])) []),
    ("default", tokenizeConsuming defaultConsoom)
  ]

defaultConsoom :: (Char -> Bool)
defaultConsoom = (`elem` ("_-" ++ ['a' .. 'z'] ++ ['A' .. 'Z']))

-- tokenizeConsuming ['a'..'z'] " abc-Def ghi" -> [" ","abc","-","D","ef"," ","ghi"]
tokenizeConsuming :: (Char -> Bool) -> String -> [String]
tokenizeConsuming consume = dropWhile null . fn
  where
    fn [] = []
    fn (x : xs) =
      if consume x
        then case fn xs of
          [] -> [[x]]
          (y : ys) -> (x : y) : ys
        else case fn xs of
          ([] : ys) -> [] : [x] : ys
          ys -> [] : [x] : ys
