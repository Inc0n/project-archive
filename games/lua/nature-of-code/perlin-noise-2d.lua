
require 'class'

Walker = Class()

local width, height = love.graphics.getDimensions()

function Walker:__init(origX, origY)
end

function Walker:step(dt)
end

function perlinStatic2d()
  local points = {}
  local brightness = 0
  for i = 1, width do
	for j = 1, height do
	  brightness = love.math.noise(i, j)
	  table.insert(points, {i, j, brightness, brightness, brightness})
	end
  end
  return points
end

function perlinNoise2d()
  local points = {}
  local brightness, tx, ty = 0, 0
  for i = 1, width do
	ty = 0.0;
	for j = 1, height do
	  brightness = love.math.noise(tx, ty)
	  table.insert(points, {i, j, brightness, brightness, brightness})
	  ty = ty + 0.01
	end
	tx = tx + 0.01
  end
  return points
end

function Walker:draw()
  local points = perlinNoise2d()
  love.graphics.points(points)
end

-- global object list
return {Walker(100, 100)}