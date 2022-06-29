
Input = require 'Input'
objects = nil
input = nil

function setup()
  print("setting up the game")
  input = Input()
  input:bind('q', function() love.event.quit() end)
end

-- load the module specified args
function love.load(args, _)
  if args[1] then
	objects = require(args[1])
	if (#objects > 0) then
	  return -- early return to avoid program termination
	end
  end
  print("run the program with a additional module to lood")
  love.event.quit()
end

-- update all the objects in the global "object" variable by calling
-- their "step" method
function love.update(dt)
  for _,o in ipairs(objects) do
	o:step(dt)
  end
  -- if love.keyboard.isDown("escape") then love.event.quit() end
end

-- draw all the objects in the global "object" variable by calling
-- their "draw" method
function love.draw()
  for _,o in ipairs(objects) do
	o:draw()
  end
end

function love.run()
  if love.load then
	love.load(love.arg.parseGameArguments(arg), argument)
	setup()
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

function love.keypressed(key, _)
  if key == "q" then love.event.quit() end
end