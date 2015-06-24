{ mkDerivation, base, cpu, network, stdenv, tasty, tasty-hunit
, template-haskell
}:
mkDerivation {
  pname = "ip-quoter";
  version = "1.0.1.0";
  src = ./.;
  buildDepends = [ base cpu network template-haskell ];
  testDepends = [ base cpu network tasty tasty-hunit ];
  homepage = "https://github.com/shlevy/ip-quoter";
  description = "Quasiquoter for IP addresses";
  license = stdenv.lib.licenses.mit;
}
