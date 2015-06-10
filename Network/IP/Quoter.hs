{-# LANGUAGE TemplateHaskell #-}
module Network.IP.Quoter (ip) where

import Data.Char ( isDigit )
import Data.Bits ( (.|.), unsafeShiftL )
import Data.List.Split ( splitOn )
import Network.Socket ( HostAddress )
import Language.Haskell.TH.Quote ( QuasiQuoter( .. ) )
import Language.Haskell.TH.Syntax ( Q, Exp( .. ), Lit( .. ), Type ( .. ) )

ip :: QuasiQuoter
ip = QuasiQuoter
  { quotePat = \_ -> fail "Can't invoke the ip quasiquoter in a pattern context"
  , quoteType = \_ -> fail "Can't invoke the ip quasiquoter in a type context"
  , quoteDec = \_ ->
               fail "Can't invoke the ip quasiquoter in a declaration context"
  , quoteExp = parseIP
  }

parseDigit :: Char -> Q Integer
parseDigit c
  | isDigit c = return . fromIntegral $ (fromEnum c - fromEnum '0')
  | otherwise = fail "Non-digit in IP address"

parseSegment' :: String -> Integer -> Q Integer
parseSegment' [] total = return total
parseSegment' (digit : rest) total
  | total <= 25 = do
      next <- parseDigit digit
      if (total == 0) && (next == 0) && (not (null rest))
        then fail "Leading zero in IP address segment"
        else parseSegment' rest (total * 10 + next)
  | otherwise = fail "IP address segment too big"

parseSegment :: String -> Q Integer
parseSegment [] = fail "Empty IP address segment"
parseSegment str = parseSegment' str 0

parseIP :: String -> Q Exp
parseIP s = case (splitOn "." s) of
  l@[ first, second, third, fourth ] -> do
      bytes <- mapM (fmap (LitE . IntegerL) . parseSegment) l
      return $ SigE (foldl1 shiftAndOr bytes) (ConT ''HostAddress)
    where
      shiftAndOr acc exp = AppE (AppE (VarE '(.|.)) shifted) exp
        where
          shifted = AppE (AppE (VarE 'unsafeShiftL) acc) (LitE $ IntegerL 8)
  _ -> fail "IP address doesn't contain four segments"
