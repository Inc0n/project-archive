require '../class'

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

--

-- global object list
local objects = {Walker(100,100)}

-- function pp(x)
--   for i,v in ipairs(x) do
-- 	if type(v) == 'table' then
-- 	  print('new table', v)
-- 	  pp(v)
-- 	else
-- 	  print(i, v)
-- 	end
--   end
-- end

function love.load(arg1, _)
  print(arg1)
  for i,v in ipairs(arg1) do
	print(i, v)
  end
end

function love.update(dt)
  for _,o in ipairs(objects) do
	o:step(dt)
  end
end

function love.draw()
  for _,o in ipairs(objects) do
	o:draw()
  end
end

function love.run()
  if love.load then
	love.load(love.arg.parseGameArguments(arg), argument)
  end

  local dt = 0
  local updateFn = love.update or (function(dt) end)

  -- Main loop time
  return function()
	-- process events.
	if love.event then
	  love.event.pump()
	  for name,a,b,c,d,e,f in love.event.poll() do
		if name == "quit" then
		  if not love.quit or not love.quit() then
			return a or 0
		  end
		end
	  end
	end
	-- Get new dt
	local dt = love.timer and love.timer.step() or 0

	-- update
	updateFn(dt)

	-- draw
	if love.graphics and love.graphics.isActive() then
	  love.graphics.origin()
	  -- love.graphics.clear(love.graphics.getBackgroundColor())
	  if love.draw then
		love.draw()
	  end
	  love.graphics.present()
	end
	if love.timer then
	  love.timer.sleep(0.001)
	end
  end
end