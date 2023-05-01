


-- perfect user interface
----- neverlose



----<  Header  >----------------------------------------------------------------

--------------------------------------------------------------------------------
-- #region: < Header >


--
-- #region : Definitions

--#region: localization

local assert, collectgarbage, error, getfenv, setfenv, getmetatable, setmetatable,
ipairs, pairs, load, next, pcall, rawequal, rawset, rawlen, require, select,
tonumber, tostring, type, unpack, xpcall, print, print_raw, print_error =
assert, collectgarbage, error, getfenv, setfenv, getmetatable, setmetatable,
ipairs, pairs, load, next, pcall, rawequal, rawset, rawlen, require, select,
tonumber, tostring, type, unpack, xpcall, print, print_raw, print_error

local function C (o)
	if type(o) ~= "table" then return o end
	local res = {} for k, v in pairs(o) do res[C(k)] = C(v) end return res
end

local table, math, string = C(table), C(math), C(string)
local ui, render, common, utils = C(ui), C(render), C(common), C(utils)

--#endregion

--#region: global table

table.find = function (t, j)  for k, v in pairs(t) do if v == j then return k end end return false  end
table.ifind = function (t, j)  for i = 1, table.maxn(t) do if t[i] == j then return i end end  end
table.ihas = function (t, ...) local arg = {...} for i = 1, table.maxn(t) do for j = 1, #arg do if t[i] == arg[j] then return true end end end return false end

table.filter = function (t)  local res = {} for i = 1, table.maxn(t) do if t[i] ~= nil then res[#res+1] = t[i] end end return res  end
table.append = function (t, ...)  for i, v in ipairs{...} do table.insert(t, v) end  end
table.appendf = function (t, ...)  local arg = {...} for i = 1, table.maxn(arg) do local v = arg[i] if v ~= nil then t[#t+1] = v end end  end
table.range = function (t, i, j)  local r = {} for l = i or 0, j or #t do r[#r+1] = t[l] end return r  end
table.copy = C

math.round = function (value)  return math.floor (value + 0.5)  end
math.lerp = function (a, b, w)  return a + (b - a) * w  end

local ternary = function (c, a, b)  if c then return a else return b end  end
local aserror = function (a, msg, level) if not a then error(msg, level and level + 1 or 4) end end
local contend = function (func, callback, ...)
	local t = { pcall(func, ...) }
	if not t[1] then if type(callback) == "function" then return callback(t[2]) else error(t[2], callback or 2) end end
	return unpack(t, 2)
end

local debug = setmetatable({
	warning = function (...)
		print_raw("\ae09334pui - ", ...)
	end,
	error = function (...)
		print_raw("\aef6060pui - ", ...)
		cvar.play:call("ui/menu_invalid.wav")
		error()
	end
}, {
	__call = function (self, ...)
		if _IS_MARKET then return end
		print_raw("\a74a6a9pui - ", ...)
		print_dev(...)
	end
})

--#endregion

--#region: directory tools

local dirs = {
	execute = function (t, path, func)
		local p, k for _, s in ipairs(path) do
			k, p, t = s, t, t[s]
			if t == nil then return end
		end
		if p[k] then func(p[k]) end
	end,
	replace = function (t, path, value)
		local p, k for _, s in ipairs(path) do
			k, p, t = s, t, t[s]
			if t == nil then return end
		end
		p[k] = value
	end,
	find = function (t, path)
		local p, k
		for _, s in ipairs(path) do
			k, p, t = s, t, t[s]
			if type(t) ~= "table" then break end
		end
		return p[k]
	end,
}

dirs.pave = function (t, place, path)
	local p = t for i, v in ipairs(path) do
		if type(p[v]) == "table" then p = p[v]
		else p[v] = (i < #path) and {} or place  p = p[v]  end
	end return t
end

dirs.extract = function (t, path)
	if not path or #path == 0 then return t end
	local j = dirs.find(t, path)
	return dirs.pave({}, j, path)
end

--#endregion

local pui, pui_mt, methods_mt = {}, {}, { element = {}, group = {} }
local tools, elemence = {}, {}
local config, is_setup = {}, false

--

local dpi = render.get_scale(1)

-- #endregion
--

--
-- #region : Elements

--#region: definitions

local elements = {
	switch					= { type = "boolean",	arg = 2 },
	slider					= { type = "number",	arg = 6 },
	combo					= { type = "string",	arg = 2, variable = true },
	selectable				= { type = "table",		arg = 2, variable = true },
	button					= { type = "function",	arg = 3, unsavable = true },
    list					= { type = "number",	arg = 2, variable = true },
    listable				= { type = "table",		arg = 2, variable = true },
    label					= { type = "string",	arg = 1, unsavable = true },
    texture					= { type = "userdata",	arg = 5, unsavable = true },
    image					= { type = "userdata",	arg = 5, unsavable = true },
    hotkey					= { type = "number",	arg = 2 },
    input					= { type = "string",	arg = 2 },
    textbox					= { type = "string",	arg = 2 },
    color_picker			= { type = "userdata",	arg = 2 },
	["sol.Lua::LuaVarClr"]	= { type = "userdata",	arg = 2 },
}

--#endregion

--#region: methods parsing

local __mt = {
	group = {}, wrp_group = {},
	element = {}, wrp_element = {}
} do
	local element = ui.find("Miscellaneous", "Main", "Movement", "Air Duck")
    local group = element:parent()

	local element_mt, group_mt = getmetatable(element), getmetatable(group)

	for k, v in next, element_mt do
		__mt.element[k], __mt.wrp_element[k] = v, function (self, ...) return v(self.ref, ...) end
	end

	for k, v in next, group_mt do
		__mt.group[k], __mt.wrp_group[k] = v, function (self, ...) return v(self.ref, ...) end
	end
end

--#endregion

--#region: weak tables

local icons = setmetatable({}, {
    -- __mode = "k",
    __index = function (self, name)
        local icon = ui.get_icon(name)
		if #icon == 0 then
			debug.warning(icon, ("<%s> icon not found"):format(name))
		end
        self[name] = icon
        return self[name]
    end
})

local groups = setmetatable({}, {
	__mode = "k",
	__index = function (self, raw)
		local key, group
		local kind = type(raw)

		if kind == "table" then
			if raw.__name == "pui::group" then return raw.ref end
			for i = 1, #raw do  raw[i] = tools.format(raw[i])  end

			key, group = raw[1] .."-".. (raw[2] or ""), ui.create(unpack(raw))
		elseif kind == "userdata" and raw.__name == "sol.Lua::LuaGroup" then
			key, group = tostring(raw), raw
		else
			raw = tools.format(raw)
			key, group = tostring(raw), ui.create(raw)
		end

		self[key] = group

		return self[key]
	end
})

--#endregion

-- #endregion
--

--
-- #region : Utils

--#region: tools

do
	tools.gradient = function (text, colors)
		local symbols, length = {}, #(text:gsub(".[\128-\191]*", "a"))
		local s = 1 / (#colors - 1)

		local i = 0
		for letter in string.gmatch(text, ".[\128-\191]*") do
			i = i + 1

			local weight = i / length
			local cw = weight / s
			local j = math.ceil(cw)
			local w = (cw / j)
			local L, R = colors[j], colors[j+1]

			local r = L.r + (R.r - L.r) * w
			local g = L.g + (R.g - L.g) * w
			local b = L.b + (R.b - L.b) * w
			local a = L.a + (R.a - L.a) * w

			symbols[#symbols+1] = ("\a%02x%02x%02x%02x%s"):format(r, g, b, a, letter)
		end

		symbols[#symbols+1] = "\aDEFAULT"

		return table.concat(symbols)
	end

	--#region: format

	local fmethods = {
		gradients = function (col, text)
			local colors = {}; for w in string.gmatch(col, "\b%x+") do
				colors[#colors+1] = color(string.sub(w, 2))
			end
			if #colors > 0 then return tools.gradient(text, colors) end
		end,
		colors = function (col)
			return pui.colors[col] and ("\a".. pui.colors[col]:to_hex()) or "\aDEFAULT"
		end
	}

	local format = function (s)
		s = string.gsub(s, "[\v\r]", { ["\v"] = "\a".. pui.accent:to_hex(), ["\r"] = "\aDEFAULT" })
		s = string.gsub(s, "([\b%x]-)%[(.-)%]", fmethods.gradients)
		s = string.gsub(s, "\a%[(.-)%]", fmethods.colors)
		s = string.gsub(s, "\f<(.-)>", icons)

		return s
	end

	tools.format = function (object, localize)
		local kind = type(object)

		if kind == "string" then
			object = format(object)

		--[[ elseif localize and kind == "table" and type(object[1]) == "string" then
			local original = format(object[1])
			for k, v in next, object do
				if k ~= 1 then ui.localize(k, original, format(v)) end
			end	object = original ]]
		end

		return object
	end

	--#endregion
end

--#endregion

--#region: elemence

do
	elemence.new = function (ref)
		local this = {
			ref = ref
		}
		--

		this.__depend = { {}, {} }
		this[0], this[1] = {
			type = __mt.element.type(this.ref)
		}, {}
		this[0].savable = not elements[this[0].type].unsavable == true
		--

		this.value = __mt.element.get(this.ref)
		if this[0].type ~= "button" then
			__mt.element.set_callback(this.ref, function (self)
				this.value = __mt.element.get(self)
			end)
		end

		return setmetatable(this, methods_mt.element)
	end

	elemence.group = function (ref)
		return setmetatable({ ref = ref, __depend = { {}, {} } }, methods_mt.group)
	end

	elemence.dispense = function (key, ...)
		local args, ctx = {...}, elements[key]

		args.n = table.maxn(args)

		local variable, counter = (ctx and ctx.variable) and type(args[2]) == "string", 1
		args.req, args.misc = (ctx and not variable) and ctx.arg or args.n, {}

		for i = 1, args.n do
            local v = args[i]
            local kind = type(v)

			if i == 2 and ctx.variable and not variable then
				for j = 1, #v do
					v[j] = tools.format(v[j], true)
				end
			else
				args[i] = tools.format(v, true)
			end

            if kind == "userdata" and v.__name == "sol.Vector" then  args[i] = v * dpi  end

			if i > args.req then
				args.misc[counter], counter = v, counter + 1
			end
		end

		return args
	end

	elemence.memorize = function (self, path)
		if type(self) ~= "table" or self.__name ~= "pui::element" then return end

		if next(self[1]) == nil and rawget(self, "color") == nil then
			if self[0].savable then
				dirs.pave(config, self.ref, path)
			end
		else
			local object = {}

			if self[0].savable then  object._S = self.ref  end

			if rawget(self, "color") then
				object._C = self.color.ref
			else
				for name, ref in pairs(self[1]) do
					if ref[0].savable then
						object[name] = rawget(ref, "color") and { _S = ref.ref, _C = ref.color.ref } or ref.ref
					end
				end
			end

			dirs.pave(config, object, path)
		end
	end

	elemence.features = function (self, args)
		if self[0].type == "image" or self[0].type == "value" then return end

		local had_tooltip = false

		for i = 1, table.maxn(args) do
			local v = args[i]
			local t = type(v)

			if t == "function" and not self[0].gear then
				local condition
				self[0].gear = methods_mt.element.create(self)
				self[1], condition = v(self[0].gear, self)
				if condition ~= nil then
					for name, element in pairs(self[1]) do
						element:depend({self, condition})
					end
				end

			elseif not rawget(self, "color") and (t == "userdata" and v.__name == "sol.ImColor") or (t == "table" and v[1].__name == "sol.ImColor") then
				local g = t == "table" and v[1] or v
				methods_mt.element.color_picker(self, g)

				if t == "table" then
					methods_mt.element.depend(self.color, {self, v[2]})
				end

			elseif not had_tooltip and t == "string" or (t == "table" and type(v[1]) == "string") then
				
				__mt.element.tooltip(self.ref, tools.format(v))
				had_tooltip = true
			end
		end
	end

	--#region: .depend

	local cases = {
		combo = function (v)
			if v[3] == true then
				return v[1].value ~= v[2]
			else
				for i = 2, #v do
					if v[1].value == v[i] then return true end
				end
			end
			return false
		end,
		list = function (v)
			if v[3] == true then
				return v[1].value ~= v[2]
			else
				for i = 2, #v do
					if v[1].value == v[i] then return true end
				end
			end
			return false
		end,
		selectable = function (v)
			return table.ihas(v[1].value, unpack(v, 2))
		end,
		listable = function (v)
			return table.ihas(v[1].value, unpack(v, 2))
		end,
		slider = function (v)
			return v[2] <= v[1].value and v[1].value <= (v[3] or v[2])
		end,
	}

	local depend = function (v)
		local condition = false

		if type(v[2]) == "function" then
			condition = v[2]( v[1] )
		else
			local f = cases[v[1][0].type]
			if f then condition = f(v)
			else condition = v[1].value == v[2] end
		end

		return condition and true or false
	end

	elemence.dependant = function (__depend, dependant, disabler)
		local count = 0

		for i = 1, #__depend do
			count = count + ( depend(__depend[i]) and 1 or 0 )
		end

		local kind = dependant.__name == "sol.Lua::LuaGroup" and "group" or "element"
		__mt[kind][disabler and "disabled" or "visibility"](dependant, count >= #__depend)
	end

	--#endregion
end


--#endregion

-- #endregion
--


-- #endregion ------------------------------------------------------------------
--






----<  Header  >----------------------------------------------------------------

--------------------------------------------------------------------------------
-- #region: < PUI >


--
-- #region : pui

--#region: variables

pui.__name = "pui::basement"
pui.accent = ui.get_style("Link Active")
pui.colors = {}
pui.alpha = ui.get_alpha()
events.render:set(function ()
	pui.accent = ui.get_style("Link Active")
	pui.alpha = ui.get_alpha()
end)

--#endregion

--#region: features

pui.string = tools.format

pui.create = function (tab, name, align)
	return elemence.group( groups[name and {tab, name, align} or tab] )
end

pui.find = function (...)
	local arg = {...}
	local children for i, v in ipairs(arg) do
		if type(v) == "table" then
			children, arg[i] = v, nil
		break end
	end

	local found = { ui.find( unpack(arg) ) }

	for i, v in ipairs(found) do
		found[i] = elemence[v.__name == "sol.Lua::LuaGroup" and "group" or "new"](v)
	end

	if found[2] and found[2].ref.__name == "sol.Lua::LuaVar" then
		found[1].color, found[2] = found[2], nil
	elseif children and found[1] then
		for k, v in pairs(children) do
			local path = {...}
			path[#path] = v
			found[1][1][k] = pui.find( unpack(path) )
		end
	end


	return found[1]
end

pui.sidebar = function (name, icon)
	name, icon = tools.format(name), icon and tools.format(icon) or nil

	ui.sidebar(name, icon)
end

pui.traverse = function (t, f, p)
	p = p or {}

	if type(t) == "table" and (t.__name ~= "pui::element" and t.__name ~= "pui::group") and t[#t] ~= "~" then
		for k, v in next, t do
			local np = table.copy(p); np[#np+1] = k
			pui.traverse(v, f, np)
		end
	else
		f(t, p)
	end
end

pui.translate = function (original, translations)
	original = tools.format(original)
	for k, v in pairs(translations or {}) do
		ui.localize(k, original, tools.format(v))
	end
	return original
end

do
	local mt = {
		create = function (self, ...)
			return elemence.group(__mt.group.create(self[1], ...))
		end
	}	mt.__index = mt

	local sidebar = ui.find("Aimbot", "Anti Aim"):parent():parent()
	local cats = {}

	pui.category = function (name, tab)
		name, tab = tostring(tools.format(name)), tostring(tools.format(tab))
		local ref = contend(ui.find, function () end, name, tab)

		if not cats[name] then
			cats[name] = {}
			if not ref then cats[name][0] = sidebar:create(name) end
		end
		if not cats[name][tab] then
			if ref then cats[name][tab] = ref
			else cats[name][tab] = cats[name][0]:create(tab) end
		end

		return setmetatable({cats[name][tab]}, mt)
	end
end

--#endregion

--#region: config system

pui.setup = function (t)
    if is_setup then return debug.warning("config is already setup by this or another script") end
	pui.traverse(t, elemence.memorize)
    is_setup = true
	return t
end

pui.save = function (...)
    if not is_setup then return debug.warning("pui.setup was not called, config not saved") end
	local packed = {}
	pui.traverse(dirs.extract(config, {...}), function (ref, path)
        local etype = __mt.element.type(ref)
		local value, value2 = __mt.element[etype == "hotkey" and "key" or "get"](ref)
		local vtype, v2type = type(value), type(value2)

		if etype == "color_picker" then
			if vtype == "table" then
				value2, v2type = value, vtype
				value, vtype = __mt.element.list(ref)[1], "string"
			end

			if value2 then
				value = { value }
				if v2type == "table" then
					for i = 1, #value2 do
						value[#value+1] = "#".. value2[i]:to_hex()
					end
				else
					value[2] = "#".. value2:to_hex()
				end
				value[#value+1] = "~"
			else
				value = "#".. value:to_hex()
			end
		elseif vtype == "table" then
			value[#value+1] = "~"
		end

		dirs.pave(packed, value, path)
	end)
	return packed
end

pui.load = function (raw, ...)
    if not is_setup then return debug.warning("pui.setup was not called, config not loaded") end
    if not raw then return end

	local packed = dirs.extract(raw, {...})
	pui.traverse(dirs.extract(config, {...}), function (ref, path)
		local value = dirs.find(packed, path)

		local multicolor
		local vtype, etype = type(value), __mt.element.type(ref)
		local object = elements[etype] or elements[ref.__name]

		if etype == "color_picker" then
			if vtype == "string" and value:sub(1, 1) == "#" then
				value = color(value)
				vtype = "userdata"
			elseif vtype == "table" then
				value[#value] = nil
				for i = 2, #value do value[i] = color(value[i]) end
				multicolor = true
				vtype = "userdata"
			end
		elseif vtype == "table" and value[#value] == "~" then
			value[#value] = nil
		end

		-- 
		if not object or object.type ~= vtype then return __mt.element.reset(ref) end

		if etype == "hotkey" then
			__mt.element.key(ref, value)
		elseif etype == "color_picker" and multicolor then
			__mt.element.set(ref, value[1])
			__mt.element.set(ref, value[1], table.range(value, 2))
		else
			__mt.element.set(ref, value)
		end
	end)
end

--#endregion

-- #endregion
--

--
-- #region : methods

methods_mt.element = {
	__type = "pui::element",
	__name = "pui::element",
	__tostring = function (self) return "pui::element.".. self[0].type .." - ".. self.name end,
	__index = function (self, key)
		return methods_mt.element[key] or __mt.wrp_element[key] or self[1][key]
	end,
	__call = function (self, ...)
		return __mt.element[#{...} == 0 and "get" or "set"](self.ref, ...)
	end,
	__eq = function (a, b) return __mt.element.__eq(a.ref, b.ref) end,

	--

	create = function (self)
		return elemence.group(__mt.element.create(self.ref))
	end,

	depend = function (self, ...)
		local arg = {...}
		local disabler = arg[1] == true

		local __depend = self.__depend[disabler and 2 or 1]
		for i = disabler and 2 or 1, table.maxn(arg) do
			local v = arg[i]
			if v then
				if v.__name == "pui::element" then v = {v, true} end

				v[0] = false
				__depend[#__depend+1] = v

				local check = function () elemence.dependant(__depend, self.ref, disabler) end
				check()

				__mt.element.set_callback(v[1].ref, check)
			end
		end
	end,

	--

	name = function (self, s)
		if s then	__mt.element.name(self.ref, tools.format(s))
		else		return __mt.element.name(self.ref) end
	end,
	set_name = function (self, s)
		__mt.element.name(self.ref, tools.format(s))
	end,
	get_name = function (self)
		return __mt.element.name(self.ref)
	end,

	type = function (self) return self[0].type end,
	get_type = function (self) return self[0].type end,

	list = function (self)
		return __mt.element.list(self.ref)
	end,
	get_list = function (self)
		return __mt.element.list(self.ref)
	end,
	update = function (self, ...)
		__mt.element.update(self.ref, ...)
        self.value = __mt.element.get(self.ref)
	end,

	tooltip = function (self, t)
		if s then	__mt.element.tooltip(self.ref, tools.format(t))
		else		return __mt.element.tooltip(self.ref) end
	end,
	set_tooltip = function (self, t)
		__mt.element.tooltip(self.ref, tools.format(t))
	end,
	get_tooltip = function (self)
		return __mt.element.tooltip(self.ref)
	end,

	set_visible = function (self, v)
		__mt.element.visibility(self.ref, v)
	end,
	get_visible = function (self)
		__mt.element.visibility(self.ref)
	end,

	get_color = function (self)
		return rawget(self, "color") and self.color.value
	end,
	color_picker = function (self, default)
		self.color = elemence.new(__mt.element.color_picker(self.ref, default))

		return self.color
	end,

	override = function (self, ...)
		__mt.element.override(self.ref, ...)
	end,
	get_override = function (self)
		return __mt.element.get_override(self.ref)
	end,
}

--

methods_mt.group = {
	__name = "pui::group",
	__index = function (self, key)
		return methods_mt.group[key] or (elements[key] and pui_mt.__index(self, key) or __mt.wrp_group[key])
	end,

	name = function (self, s)
		return __mt.group.name(self.ref, s and tools.format(s) or nil)
	end,
	set_name = function (self, s)
		__mt.group.name(self.ref, tools.format(s))
	end,
	get_name = function (self)
		return __mt.group.name(self.ref)
	end,

	set_visible = function (self, b)
		for i, v in ipairs(self[1]) do
			__mt.element.visibility(v, b)
		end
	end,

	depend = methods_mt.element.depend
}

-- #endregion
--

--
-- #region : pui_mt

pui_mt.__index = function (self, key)
	if not elements[key] then return ui[key] end

	return function (origin, ...)
		local is_child = self.__name == "pui::group"
		local group = is_child and origin.ref or groups[origin]

		local args = elemence.dispense(key, ...)
		local this = elemence.new( __mt.group[key]( group, unpack(args, 1, args.n < args.req and args.n or args.req) ) )

		elemence.features(this, args.misc)

		return this
	end
end

-- #endregion
--


-- #endregion ------------------------------------------------------------------
--




return setmetatable(pui, pui_mt)

------------------------------------------------------------<  enQ • 2023  >----
