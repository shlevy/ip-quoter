{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Network.IP.Quoter (ip) where

import Network.Socket ( HostAddress
                      , HostAddress6
                      , SockAddr( .. )
                      , getAddrInfo
                      , AddrInfo ( addrAddress, addrFlags )
                      , AddrInfoFlag ( AI_NUMERICHOST )
                      , defaultHints
                      )
import Language.Haskell.TH.Quote ( QuasiQuoter( .. ) )
import Language.Haskell.TH.Syntax ( Q
                                  , Exp( .. )
                                  , Lit( .. )
                                  , Type ( .. )
                                  , runIO
                                  )
import Data.Word ( Word32 )
import System.Endian ( fromBE32, toBE32 )

-- | QuasiQuoter for ip addresses (e.g. '[ip|127.0.0.1|]')
ip :: QuasiQuoter
ip = QuasiQuoter
  { quotePat = \_ -> fail "Can't invoke the ip quasiquoter in a pattern context"
  , quoteType = \_ -> fail "Can't invoke the ip quasiquoter in a type context"
  , quoteDec = \_ ->
               fail "Can't invoke the ip quasiquoter in a declaration context"
  , quoteExp = parseIP
  }

getIPInfo :: String -> IO SockAddr
getIPInfo s = do
    best:_ <- getAddrInfo hint hostname Nothing
    return $ addrAddress best
  where
    hint = Just $ defaultHints { addrFlags = [ AI_NUMERICHOST ] }
    hostname = Just s

parseIP :: String -> Q Exp
parseIP s = (runIO $ getIPInfo s) >>= \case
  SockAddrInet _ addr ->
      return . SigE networkExp $ ConT ''HostAddress
    where
      hostW32 = fromBE32 addr
      w32Lit = LitE . IntegerL $ fromIntegral hostW32
      toBE32Var = VarE 'toBE32
      networkExp = AppE toBE32Var w32Lit

  SockAddrInet6 _ _ ( addr1, addr2, addr3, addr4 ) _ ->
      return . SigE tup $ ConT ''HostAddress6
    where
      tup = TupE $ map (LitE . IntegerL . fromIntegral) [ addr1
                                                        , addr2
                                                        , addr3
                                                        , addr4
                                                        ]

  x -> fail ("Invalid address " ++ (show x) ++ " when parsing " ++ s)
