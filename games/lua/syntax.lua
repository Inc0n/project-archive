dirs = {{1,0},{0,1},{-1,0},{0,-1},{0,0}}
ndirs = {{1,0},{0,-1},{-1,0},{0,1},{0,0}}
materials = {"rock","lava","wall","ice","player","monster","spike","water","error","skull","grass","flag","empty","metal","heart","anni"}
aspects = {"slip","win","push","stop","sink","move","best","melt","hot","you","grab","kill"}

features = {}

function clear()
  local player = findfeature("is you")
  features = {}

  if (player ~= nil) then
	for a,b in ipairs(player) do
	  local name = b
	  --table.insert(features, {name,"is stop"})
	end
  end
end

function movecommand(ox,oy,dir_)
  local p = findfeature("is you")
  local slip = findfeature("is slip")

  if (p ~= nil) then
	for a,b in ipairs(p) do
	  local players = MF_findall(b)

	  for i,v in ipairs(players) do
		local x,y,dir = MF_pos(v)
		local slipping = 0

		if (slip ~= nil) then
		  for d,c in ipairs(slip) do
			local ice = MF_findtype(c,x,y,v)

			if (ice > 0) then
			  slipping = 1
			end
		  end
		end

		if (slipping == 0) then
		  move(v,ox,oy,dir_)
		else
		  local drs = ndirs[dir+1]
		  local oox = drs[1]
		  local ooy = drs[2]
		  local hm = check(v,oox,ooy,dir)

		  if (hm >= 1) then
			move(v,ox,oy,dir_)
		  end
		end
	  end
	end
  end
end

function move(unit,ox,oy,dir_)
  local x,y,dir = MF_pos(unit)
  local name = MF_name(unit)

  if (dir_ ~= nil) then
	dir = dir_
  end

  if (ox ~= 0) or (oy ~= 0) then
	local hm = check(unit,ox,oy,dir)
	local done = false

	while (done == false) do
	  if (hm == 0) then
		local grab = findfeature("is grab")
		MF_update(unit,x+ox,y+oy,dir)
		done = true

		if (grab ~= nil) then
		  for a,b in ipairs(grab) do
			if (b == name) then
			  local push = findfeature("is push")

			  for i=1,#ndirs do
				local dr = ndirs[i]
				local oox = dr[1]
				local ooy = dr[2]
				if (push ~= nil) then
				  for c,d in ipairs(push) do
					local target = MF_findtype(d,x+oox,y+ooy,unit)

					if (target > 0) and (target ~= unit) then
					  move(target,ox,oy,dir)
					end
				  end
				end

				target = MF_findtext(x+oox,y+ooy)

				if (target > 0) and (target ~= unit) then
				  move(target,ox,oy,dir)
				end
			  end
			end
		  end
		end
	  elseif (hm > 1) then
		hm = move(hm,ox,oy,dir)
		MF_sound(1)
	  else
		done = true
	  end
	end

	return hm
  end
end

function check(unit,ox,oy)
  local x,y = MF_pos(unit)
  local result_ = MF_obstacle(x+ox,y+oy)
  local final = 0
  local emptypush = findfeature("empty is push")
  local emptystop = findfeature("empty is stop")

  if (#result_ > 0) then
	for i,v in ipairs(result_) do
	  local result = v
	  if (result > 1) then
		local type = MF_gettype(result)
		local r2 = "done"
		local r3 = "done"

		if (type ~= "text") then
		  local name = MF_name(result)
		  r2 = findfeature(name .. " is push")
		  r3 = findfeature(name .. " is stop")
		end

		if (r2 ~= nil) then
		  if (final ~= 1) and (final ~= -1) then
			final = result
		  end
		else
		  if (r3 ~= nil) then
			final = 1
		  end
		end
	  end

	  if (v == -1) then
		final = 1
	  end
	end
  end

  if (final == 0) and (emptystop ~= nil) then
	final = 1
  elseif (final == 0) and (emptypush ~= nil) then
	final = 1
  end

  return final
end

function code(unit,ox_,oy_)
  local x,y = MF_pos(unit)
  local unittype = MF_type(unit)
  local i_ = 2
  local ox,oy = 0,0

  if (ox_ ~= nil) and (oy_ ~= nil) then
	ox,oy = ox_,oy_
	i_ = 1
  end

  for i=1,i_ do
	local finalresult = {}
	if (i_ > 1) then
	  local d = dirs[i]
	  ox = d[1]
	  oy = d[2]
	end

	local result = MF_name(unit)
	table.insert(finalresult, result)
	local bits = 1

	if (ox ~= 0) or (oy ~= 0) then
	  local hm = codecheck(unit,ox,oy)
	  local done = false

	  while (done == false) do
		if (hm == 0) then
		  done = true
		elseif (hm > 0) then
		  local r = result
		  hm,r = code(hm,ox,oy)
		  result = result .. " " .. r
		  table.insert(finalresult, r)
		  bits = bits + 1
		end
	  end

	  if (ox_ ~= nil) and (oy ~= nil) then
		return hm,result
	  else
		if (unittype == 0) and (bits > 1) then
		  table.insert(features, finalresult)
		end
	  end
	end
  end
end

function codecheck(unit,ox,oy)
  local x,y = MF_pos(unit)
  local result = MF_code(x+ox,y+oy)

  return result
end

function block(unit,name)
  local x,y,dir,moved = MF_pos(unit)
  local ox,oy = 0,0

  local isice = findfeature("is melt")
  local ismove = findfeature("is move")
  local isyou = findfeature("is you")
  local ispush = findfeature("is push")
  local iskill = findfeature("is kill")
  local iswin = findfeature("is win")
  local isbest = findfeature("is best")

  local isice_ = isthis(name,isice)
  local ismove_ = isthis(name,ismove)
  local isyou_ = isthis(name,isyou)
  local ispush_ = isthis(name,ispush)
  local iskill_ = isthis(name,iskill)
  local iswin_ = isthis(name,iswin)
  local isbest_ = isthis(name,isbest)

  local slipping = 0
  if (name == ismove_) or (name == isyou_) or (name == ispush_) then
	local slip = findfeature("is slip")

	if (slip ~= nil) then
	  for a,b in ipairs(slip) do
		local ice = MF_findtype(b,x,y,unit)

		if (ice > 0) then
		  slipping = 1
		end
	  end

	  if (slipping == 1) then
		local d = ndirs[dir+1]
		ox = d[1]
		oy = d[2]

		if (moved == 0) then
		  local boop = move(unit,ox,oy)
		  x,y,dir = MF_pos(unit)
		  --MF_extratime(unit)
		end
	  end
	end
  end

  if (name == ismove_) and (slipping == 0) then
	local d = ndirs[dir+1]
	ox = d[1]
	oy = d[2]

	local boop = move(unit,ox,oy)

	if (boop == 1) then
	  MF_updatedir(unit,rotate(dir))
	  move(unit,ox,oy)
	end
	x,y,dir = MF_pos(unit)
  end

  if (name == ispush_) or (name == ismove_) or (name == isyou_) then
	local sink = findfeature("is sink")

	if (sink ~= nil) then
	  for a,b in ipairs(sink) do
		local water = MF_findtype(b,x,y,unit)

		if (water > 0) then
		  MF_remove(unit)
		  MF_remove(water)
		end
	  end
	end
  end

  if (name == isyou_) then
	local win = findfeature("is win")

	if (win ~= nil) then
	  for a,b in ipairs(win) do
		if (b == "empty") then
		  local flag = MF_findtype(b,x,y,unit)

		  if (flag > 0) then
			MF_win()
		  end
		end
	  end
	end

	local kill = findfeature("is kill")

	if (kill ~= nil) then
	  for a,b in ipairs(kill) do
		if (b == "empty") then
		  local skull = MF_findtype(b,x,y,unit)

		  if (skull > 0) then
			MF_remove(unit)
		  end
		end
	  end
	end
  end

  if (name == isice_) then
	local hot = findfeature("is hot")

	if (hot ~= nil) then
	  for a,b in ipairs(hot) do
		local lava = MF_findtype(b,x,y,unit)

		if (lava > 0) then
		  MF_remove(unit)
		end
	  end
	end
  end

  if (name == iskill_) then
	local player = findfeature("is you")

	if (player ~= nil) then
	  for a,b in ipairs(player) do
		local skull = MF_findtype(b,x,y,unit)

		if (skull > 0) then
		  MF_remove(skull)
		end
	  end
	end
  end

  if (name == iswin_) then
	local player = findfeature("is you")

	if (player ~= nil) then
	  for a,b in ipairs(player) do
		local flag = MF_findtype(b,x,y,unit)

		if (flag > 0) then
		  MF_win()
		end
	  end
	end
  end

  if (name == isbest_) then
	MF_bling(unit)
  end
end

function emptyisbest()
  local emptyisbest = findfeature("empty is best")

  if (emptyisbest ~= nil) then
	MF_blingempty()
  end
end

function rules()
  for i,v in ipairs(features) do
	local deb = ""
	local full = ""
	local limit = 0

	for a,b in ipairs(v) do
	  full = full .. b .. " "
	end

	local words = {}
	local cword = ""
	for i=1,string.len(full) do
	  if (string.sub(full, i, i) ~= " ") then
		cword = cword .. string.sub(full, i, i)
	  end

	  if (string.sub(full, i, i) == " ") or (i == string.len(full)) then
		table.insert(words, cword)
		deb = deb .. " " .. cword
		cword = ""
		limit = limit + 1
	  end

	  if (limit == 3) then
		break
	  end
	end

	--print(deb)

	if (words[2] == "is") then
	  local v3 = words[3]

	  local mat1 = 0

	  if (words[1] ~= "empty") then
		mat1 = MF_material(words[1])
	  else
		mat1 = 1
	  end

	  local mat2 = 0

	  for a,b in ipairs(materials) do
		if (b == v3) then
		  mat2 = 1
		end
	  end

	  if (mat1 == 1) and (mat2 == 1) then
		MF_convert(words[1],v3)
	  end
	end
  end
end

function findfeature(rule)
  local one,two,three = "","",""
  local result = {}

  for i,v in ipairs(features) do
	local full = ""
	local limit = 0
	for a,b in ipairs(v) do
	  full = full .. b .. " "
	end

	local words = {}
	local cword = ""
	for i=1,string.len(full) do
	  if (string.sub(full, i, i) ~= " ") then
		cword = cword .. string.sub(full, i, i)
	  end

	  if (string.sub(full, i, i) == " ") or (i == string.len(full)) then
		table.insert(words, cword)
		cword = ""
		limit = limit + 1
	  end

	  if (limit == 3) then
		break
	  end
	end

	if (#words >= 3) then
	  one = words[3]
	  two = words[2] .. " " .. words[3]
	  three = words[1] .. " " .. words[2] .. " " .. words[3]

	  if (one == rule) or (two == rule) or (three == rule) then
		table.insert(result, v[1])
	  end
	end
  end

  if (#result > 0) then
	return result
  else
	return nil
  end
end

function rotate(dir)
  local rot = {2,3,0,1}

  return rot[dir+1]
end

function loadtile(x,y)
  local key = tostring(x) .. "," .. tostring(y)

  if (tiles[key] ~= nil) then
	return tiles[key]
  else
	return "error"
  end
end

function isthis(name,what)
  if (what ~= nil) then
	for a,b in ipairs(what) do
	  if (name == b) then
		return b
	  end
	end
  end

  return ""
end

tiles =
  {
	["2,0"] = "player",
	["3,0"] = "monster",
	["4,0"] = "rock",
	["5,0"] = "text_grass",
	["6,0"] = "metal",
	["2,1"] = "lava",
	["3,1"] = "water",
	["4,1"] = "wall",
	["5,1"] = "text_empty",
	["6,1"] = "text_metal",
	["0,2"] = "text_player",
	["1,2"] = "text_monster",
	["2,2"] = "text_flag",
	["3,2"] = "flag",
	["4,2"] = "ice",
	["5,2"] = "text_slip",
	["6,2"] = "anni",
	["0,3"] = "text_rock",
	["1,3"] = "text_lava",
	["2,3"] = "text_wall",
	["3,3"] = "text_ice",
	["4,3"] = "text_is",
	["5,3"] = "text_anni",
	["0,4"] = "text_water",
	["1,4"] = "text_win",
	["2,4"] = "text_push",
	["3,4"] = "text_stop",
	["4,4"] = "text_move",
	["5,4"] = "text_best",
	["0,5"] = "text_melt",
	["1,5"] = "text_hot",
	["2,5"] = "text_you",
	["3,5"] = "text_not",
	["4,5"] = "text_sink",
	["5,5"] = "heart",
	["0,6"] = "text_grab",
	["1,6"] = "text_kill",
	["2,6"] = "skull",
	["3,6"] = "grass",
	["4,6"] = "text_skull",
	["5,6"] = "text_heart",
  }