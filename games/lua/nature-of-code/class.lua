-- class.lua
-- compatible with Lua 5.1

function Class(base)
  local c = {}
  if type(base) == 'table' then
	-- our new class is a shallow copy of the base class!
	for i,v in pairs(base) do
	  c[i] = v
	end
	c.__base = base
  end
  -- the class will be the metatable for all its objects and they will
  -- look up their methods in it.
  c.__index = c

  -- expose a constructor which can be called by <classname>(<args>)
  local mt = {
	__call = function(class_tbl, ...)
	  local obj = {}
	  setmetatable(obj,c)
	  if class_tbl.__init then
		class_tbl.__init(obj,...)
	  else
		-- make sure that any stuff from the base class is initialized!
		if base and base.__init then
		  base.__init(obj, ...)
		end
	  end
	  return obj
	end
  }
  setmetatable(c, mt)
  c.is_class = function(kclass)
	local m = c
	while m and m ~= klass do
	  m = m.__base
	end
	return m == klass
  end
  return c
end