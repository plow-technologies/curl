-- |
-- Module    : Network.Curl
-- Copyright : (c) 2007-2009, Galois Inc
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- A Haskell binding the libcurl library <http://curl.haxx.se/>, a
-- proven and feature-rich library for interacting with HTTP(S)\/FTP
-- servers.
--
-- The binding was initially made against version 7.16.2; libcurl does
-- appear to be considerate in not introducing breaking changes wrt
-- older versions. So, unless you're after the latest features (i.e.,
-- constructors towards the end the Option type), there's a very good
-- chance your code will work against older installations of libcurl.
module Network.Curl where

