-- |

module YAHDL.HTTP.API where

import YAHDL.HTTP.Route

import           YAHDL.Types.Snowflake
import           YAHDL.Types.General

test' = let r   = (ID :: ID Guild) !:! (ID :: ID Channel) !:! SFragment "Aaa" !:! mkRouteBuilder POST
            r'  = giveID r (Snowflake @Channel 1234)
            r'' = giveID r' (Snowflake @Guild 4321)
            b   = buildRoute r''
        in r
