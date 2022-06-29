
require 'class'
Vector = require 'vector'

Mover = Class()

local width, height = love.graphics.getDimensions()

function Mover:__init(origX, origY, m)
  self.mass = m or 1
  self.loc = Vector(origX, origY)
  self.vel = Vector()
  self.acc = Vector()
end

function Mover:applyForce(f)
  -- undefined
  if self.mass and self.acc then
	self.acc = self.acc + f / self.mass
  end
end

function Mover:applyFriction(f)
end

function Mover:checkEdge()
  if (self.loc.x > width) then
	self.loc.x = width
	self.vel.x = self.vel.x * -1
  elseif (self.loc.x < 0) then
	self.loc.x = 0
	self.vel.x = self.vel.x * -1
  end
  if self.loc.y > height then
	self.vel.y = self.vel.y * -1
	self.loc.y = height
  end
end

-- function Mover:step(dt)
--   --
--   local x,y = love.mouse.getPosition()
--   local mouse = Vector(x, y)

--   local dir = (mouse - self.loc):normalize() * 0.5
--   self.acc = dir

--   -- self.vel = self.vel + self.acc
--   self.vel = (self.vel + acc):limit(8) -- limit speed to 10 unit
--   self.loc = self.loc + self.vel
-- end

-- function Mover:draw()
--   local loc = self.loc
--   love.graphics.circle('fill', loc.x, loc.y, 10)
-- end

return Mover