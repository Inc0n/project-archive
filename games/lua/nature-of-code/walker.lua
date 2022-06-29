require 'class'

Walker = Class()

function Walker:__init(orignX, orignY)
  self.x = orignX
  self.y = orignY
end

function Walker:step()
  local dx = love.math.random(-1,1)
  local dy = love.math.random(-1,1)
  -- print(dx, dy)
  self.x = self.x + dx
  self.y = self.x + dy
end

function Walker:draw()
  local x = self.x
  local y = self.y
  love.graphics.polygon('fill', x-2, y-2, x, y-2, x+2, y+2, x-2, y+2)
  -- love.graphics.points(self.x, self.y)
end

-- global object list
return {Walker(100,100)}