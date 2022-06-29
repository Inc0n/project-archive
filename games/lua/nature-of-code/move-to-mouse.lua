
require 'class'
Vector = require 'vector'

Mover = Class()

local width, height = love.graphics.getDimensions()

function Mover:__init(origX, origY)
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

function Mover:step(dt)
  --
  local x,y = love.mouse.getPosition()
  local mouse = Vector(x, y)

  local dir = (mouse - self.loc):normalize() * 0.5
  self.acc = dir

  -- self.vel = self.vel + self.acc
  self.vel = (self.vel + acc):limit(8) -- limit speed to 10 unit
  self.loc = self.loc + self.vel
end

function Mover:draw()
  local loc = self.loc
  love.graphics.circle('fill', loc.x, loc.y, 10)
end

-- global object list
return {Mover(100, 100)}