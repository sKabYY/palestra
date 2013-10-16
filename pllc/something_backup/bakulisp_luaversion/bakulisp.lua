#!/usr/bin/env lua

require('bakulib')

-- main ---------------------------------------------------
function main(arg)
	for i, v in pairs(arg) do
		print(i .. ': ' .. v)
	end
end
main(arg)
-----------------------------------------------------------
