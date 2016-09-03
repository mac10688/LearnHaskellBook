module Ip6AddressParser where
--7. Same as before, but IPv6.


data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord, Show)

--Example IPv6 addresses and their decimal representations:
--
--0:0:0:0:fff:ac10:fe01 -> 281473568538113
--
--0:0:0:0:0:ffff:cc78:f -> 281474112159759
--
--FE80:0000:0000:0000:0202:B3FF:FE1E:8329 
-- -> 338288524927261089654163772891438416681
--
--2001:DB8::8:800:200C:417A -> 42540766411282592856906245548098208122
--
--One of the trickier parts about IPv6 will be full vs. collapsed addresses and the abbreviations. See this Q&A thread about IPv6 abbreviations for more.
--Ensure you can parse abbreviated varaitions of the earlier examples like:
--
--FE80::0202:B3FF:FE1E:8329
--2001:DB8::8:800:200C:417A
