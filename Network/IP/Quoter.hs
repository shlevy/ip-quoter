module Network.IP.Quoter (ip) where

import Language.Haskell.TH.Quote ( QuasiQuoter( .. ) )
import Language.Haskell.TH.Syntax ( Q, Exp )

ip :: QuasiQuoter
ip = QuasiQuoter
  { quotePat = \_ -> fail "Can't invoke the ip quasiquoter in a pattern context"
  , quoteType = \_ -> fail "Can't invoke the ip quasiquoter in a type context"
  , quoteDec = \_ ->
               fail "Can't invoke the ip quasiquoter in a declaration context"
  , quoteExp = parseIP
  }

parseIP :: String -> Q Exp
parseIP _ = error "Not yet implemented"
