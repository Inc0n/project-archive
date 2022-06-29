
require 'class'

local Vector = Class()

function Vector:__init(x, y)
  self.x = x or 0
  self.y = y or 0
end

function Vector.__add(self, v)
  if v.is_class(Vector) then
	return Vector(self.x+v.x, self.y+v.y)
  else
	return Vector(self.x+v, self.y+v)
  end
end

function Vector.__sub(v1,v2)
  if v2.is_class(Vector) then
	return Vector(v1.x-v2.x, v1.y-v2.y)
  else
	return Vector(v1.x-v2, v1.y-v2)
  end
end

function Vector.__div(self, v)
  return Vector(self.x/v, self.y/v)
end

function Vector.__mul(self, v)
  return Vector(self.x*v, self.y*v)
end

function Vector.magnitude(self)
  if not self._magnitude then
	self._magnitude = math.sqrt(self:dot(self))
  end
  return self._magnitude
end

function Vector.phase(self)
  if not self._phase then
	self._phase = math.atan(self.y/self.x)
  end
  return self._phase
end

function Vector.normalize(self)
  if self:magnitude() ~= 0 then
	return self / self:magnitude()
  end
  return {table.unpack(self)} -- shallow copy table
end

function Vector.limit(v, n)
  return v:normalize() * n
end

function Vector.dist(self, v)
  local vDiff = self - v
  return sqrt(vDiff:dot(vDiff))
end

function Vector.dot(self, v)
  return self.x*v.x + self.y*v.y
end

function Vector.angle(self, v)
  return -1
end
-- mt.__mul = function (self, v)
--   return Vector.Vec2(self.x*)
-- end
-- mt.__index = function(v, key)
--   if tonumber(key) ~= nil then
-- 	if key == 1 then return v["x"]
-- 	elseif key == 2 then return v["y"]
-- 	end
--   else
-- 	return v[key]
--   end
-- end

return Vector