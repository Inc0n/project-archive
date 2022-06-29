
require 'class'

Circle = Class()

function Circle:__init(radius)
  self.r = radius
end

function Circle:draw(x, y)
  love.graphics.circle('fill', x, y, self.r)
end

Walker = Class()

local width, height = love.graphics.getDimensions()

function Walker:__init(origX, origY)
  self.x = origX
  self.y = origY
  self.tx = 0
  self.ty = 1000
end

function Walker:step(dt)
  self.x = love.math.noise(self.tx) * width
  self.y = love.math.noise(self.ty) * height

  self.tx = self.tx + 0.008
  self.ty = self.ty + 0.008
end

function Walker:draw()
  local x = self.x
  local y = self.y
  love.graphics.setColor(1, 1, 1)
  love.graphics.circle('fill', x, y, 10)
  love.graphics.setColor(68/255, 68/255, 68/255)
  love.graphics.setLineWidth(2)
  love.graphics.circle('line', x, y, 10)
  -- love.graphics.points(self.x, self.y)
end

-- global object list
return {Walker(100, 100)}