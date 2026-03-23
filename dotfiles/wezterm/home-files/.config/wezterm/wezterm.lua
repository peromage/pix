--- wezterm.lua --- Wezterm config -*- lua-indent-level: 2; outline-regexp: "---\\(-* [^ \t\n]\\)"; -*-

--
-- Thie configuration accepts a custom file to apply customizations on top of
-- the default settings without touching this file itself.
--
-- The override function should return a final config table.
--
-- Example: wezterm-override.lua
--
-- local gnome_fix = require "mymodules.wayland-gnome-fix"
-- return {
--   override = function(cfg)
--     gnome_fix.apply_to_config(cfg)
--
--     cfg.font_size = 16
--     cfg.default_prog = { "fish", "-i" }
--
--     cfg.ssh_domains:_iconcat {
--       {
--         name = "Dev Domain",
--         remote_address = "dev",
--         remote_wezterm_path = "/home/fang/bin/wezterm",
--       },
--     }
--
--     cfg.wsl_domains:_iconcat {
--       {
--         name = "WSL::Ubuntu-20.04",
--         distribution = "Ubuntu-20.04",
--         default_cwd = "~",
--       },
--     }
--
--     cfg.launch_menu:_iconcat {
--       {
--         label = "SSH to dev desktop",
--         args = { "ssh", "-t", "dev" },
--         domain = { DomainName = "local" },
--       },
--     }
--
--     return cfg
--   end
-- }
--

local io = require "io"
local os = require "os"
local wezterm = require "wezterm"
local act = wezterm.action

--------------------------------------------------------------------------------
--- Utility
--------------------------------------------------------------------------------
local util
util = {
  platform = wezterm.target_triple == "x86_64-pc-windows-msvc" and "win" or "*nix",

  -- Load a builtin color scheme and return the object with some personal flavors.
  -- Returned value is a color scheme object.
  custom_color_scheme = function(scheme_name)
    local scheme = wezterm.get_builtin_color_schemes()[scheme_name]
    -- Make the scrollbar more visible (lightness less than 0.6 considered as
    -- a dark theme)
    local h, s, l, a = wezterm.color.parse(scheme.background):hsla()
    scheme.scrollbar_thumb = wezterm.color.from_hsla(h, s, l < 0.6 and 1 or 0, a)
    return scheme
  end,

  -- Increment/decrement the value based on the step.
  -- The returned value always falls between min and max.
  -- If input value is out of range, it will be stepped from the closest boundary.
  step = function(val, min, max, step)
    local res = val + step
    return (res < min and min) or (res > max and max) or res
  end,

  -- Change by percentage
  step_percentage = function(current, percent)
    return util.step(current, 0.0, 1.0, percent)
  end,
}

--------------------------------------------------------------------------------
--- Meta tables
--------------------------------------------------------------------------------
-- This meta table provides methods to manipulate tables in functional ways as
-- close as possible.
--
-- Declare lexical scope first, otherwise referencing to it is not possible.
local BasicTable
BasicTable = {
  -- Enable a table to be overridable
  _new = function(self, tbl)
    setmetatable(tbl, { __index = self })
    return tbl
  end,

  -- Shallow copy
  _copy = function(self)
    local tmp = {}
    for k, v in pairs(self) do
      tmp[k] = v
    end
    return tmp
  end,

  -- Merge keys and values from the given table and return a copy
  _merge = function(self, tbl)
    return BasicTable:_new(BasicTable._copy(self)):_imerge(tbl)
  end,

  -- Like `_merge()' but alter the original table
  _imerge = function(self, tbl)
    for k, v in pairs(tbl) do
      self[k] = v
    end
    return self
  end,

  -- Methods when the table is used as an array

  -- The list type should have at least one index
  _is_array = function(self)
    return 0 ~= #self
  end,

  -- Shallow copy by index
  _copy_as_array = function(self)
    local tmp = {}
    for _, v in ipairs(self) do
      table.insert(tmp, v)
    end
    return tmp
  end,

  -- Concatenate the given array and return a copy
  _concat = function(self, arr)
    return BasicTable:_new(BasicTable._copy_as_array(self)):_iconcat(arr)
  end,

  -- Like `_concat()' but alter the original array
  _iconcat = function(self, arr)
    for _, v in ipairs(arr) do
      table.insert(self, v)
    end
    return self
  end,
}

--------------------------------------------------------------------------------
--- Custom actions
--------------------------------------------------------------------------------
local v_split_with_current = act.SplitVertical { domain = "CurrentPaneDomain" }
local h_split_with_current = act.SplitHorizontal { domain = "CurrentPaneDomain" }
local exit_copy_mode = act.Multiple {
  act.ScrollToBottom,
  act.CopyMode "Close",
}

local function adjust_window_opacity(step)
  return function(window, pane)
    local overrides = window:get_config_overrides() or {}
    overrides.window_background_opacity =
      util.step_percentage(overrides.window_background_opacity
                           and overrides.window_background_opacity
                           or 1.0,
                           step)
    window:set_config_overrides(overrides)
  end
end

local function pane_auto_split(window, pane)
  local dimensions = pane:get_dimensions()
  -- As mono fonts usually have a width/height ratio in 1:2, we double the
  -- number of row to make the pane dimension close to visual.
  local width = dimensions.cols
  local height = dimensions.viewport_rows * 2
  if height >= width then
    window:perform_action(v_split_with_current, pane)
  else
    window:perform_action(h_split_with_current, pane)
  end
end

local function edit_scrollback(window, pane)
  -- From: https://wezterm.org/config/lua/pane/get_lines_as_text.html?h=get_line
  local tmpfile = os.tmpname()
  local fh = io.open(tmpfile, "w+")
  fh:write(pane:get_lines_as_text(pane:get_dimensions().scrollback_rows))
  fh:flush()
  fh:close()

  window:perform_action(
    act.SpawnCommandInNewTab {
      args = {"vim", tmpfile }
    },
    pane
  )

  wezterm.sleep_ms(1000)
  os.remove(tmpfile)
end

local function break_pane(window, pane)
  pane:move_to_new_tab()
  pane:activate()
end

--------------------------------------------------------------------------------
--- Keybindings
--------------------------------------------------------------------------------
--conf.leader = { mods = "CTRL", key = "`" },
local keys = BasicTable:_new {
  -- Major
  { mods = "CTRL",        key = "Tab",    action = act.ActivateTabRelative(1) },
  { mods = "CTRL|SHIFT",  key = "Tab",    action = act.ActivateTabRelative(-1) },
  { mods = "CTRL|SHIFT",  key = "c",      action = act.CopyTo "Clipboard" },
  { mods = "CTRL|SHIFT",  key = "v",      action = act.PasteFrom "Clipboard" },
  { mods = "CTRL|SHIFT",  key = "u",      action = act.CharSelect { copy_on_select = true, copy_to = "ClipboardAndPrimarySelection" }},
  { mods = "CTRL|SHIFT",  key = "p",      action = act.ActivateCommandPalette },
  { mods = "CTRL|SHIFT",  key = "g",      action = act.ShowLauncher },
  { mods = "CTRL|SHIFT",  key = "d",      action = act.ShowDebugOverlay },
  { mods = "CTRL|SHIFT",  key = "F5",     action = act.ReloadConfiguration },
  { mods = "ALT",         key = "Enter",  action = act.ToggleFullScreen },

  -- Mode shift
  { mods = "CTRL|SHIFT",  key = "x",  action = act.ActivateCopyMode },
  { mods = "CTRL|SHIFT",  key = "s",  action = act.Search "CurrentSelectionOrEmptyString" },
  { mods = "CTRL|SHIFT",  key = "z",  action = act.QuickSelect },
  { mods = "CTRL|SHIFT",  key = "e",  action = wezterm.action_callback(edit_scrollback) },

  -- Tabs
  { mods = "CTRL|SHIFT",  key = "t",  action = act.SpawnTab "CurrentPaneDomain" },
  { mods = "CTRL|SHIFT",  key = "w",  action = act.CloseCurrentTab { confirm = true }},
  { mods = "CTRL|SHIFT",  key = "l",  action = act.ActivateTabRelative(1) },
  { mods = "CTRL|SHIFT",  key = "h",  action = act.ActivateTabRelative(-1) },
  { mods = "CTRL|SHIFT",  key = ".",  action = act.MoveTabRelative(1) },
  { mods = "CTRL|SHIFT",  key = ",",  action = act.MoveTabRelative(-1) },

  -- Panes
  { mods = "CTRL|SHIFT",      key = "j",      action = act.ActivatePaneDirection "Next" },
  { mods = "CTRL|SHIFT",      key = "k",      action = act.ActivatePaneDirection "Prev" },
  { mods = "CTRL|SHIFT",      key = "1",      action = act.TogglePaneZoomState },
  { mods = "CTRL|SHIFT",      key = "2",      action = v_split_with_current },
  { mods = "CTRL|SHIFT",      key = "3",      action = h_split_with_current },
  { mods = "CTRL|SHIFT",      key = "Enter",  action = wezterm.action_callback(pane_auto_split) },
  { mods = "CTRL|SHIFT",      key = "o",      action = act.PaneSelect { mode =  "Activate" }},
  { mods = "CTRL|SHIFT",      key = "m",      action = act.RotatePanes "Clockwise" },
  { mods = "CTRL|SHIFT|ALT",  key = "m",      action = act.PaneSelect { mode =  "SwapWithActive" }},
  { mods = "CTRL|SHIFT|ALT",  key = "t",      action = wezterm.action_callback(break_pane) },
  { mods = "CTRL|SHIFT",      key = "q",      action = act.CloseCurrentPane { confirm = true }},

  -- Workspace
  { mods = "CTRL|SHIFT",  key = "]",  action = act.SwitchWorkspaceRelative(1) },
  { mods = "CTRL|SHIFT",  key = "[",  action = act.SwitchWorkspaceRelative(-1) },

  -- Window adjustment
  -- Windows
  { mods = "CTRL|SHIFT",      key = "n",  action = act.SpawnWindow },
  { mods = "CTRL|SHIFT|ALT",  key = "+",  action = wezterm.action_callback(adjust_window_opacity(0.1)) },
  { mods = "CTRL|SHIFT|ALT",  key = "_",  action = wezterm.action_callback(adjust_window_opacity(-0.1)) },
  { mods = "CTRL|SHIFT",      key = "0",  action = act.ResetFontSize },
  { mods = "CTRL|SHIFT",      key = "+",  action = act.IncreaseFontSize },
  { mods = "CTRL|SHIFT",      key = "_",  action = act.DecreaseFontSize },

  -- Pane adjustment
  { mods = "CTRL|SHIFT",  key = "LeftArrow",   action = act.AdjustPaneSize { "Left",  1}},
  { mods = "CTRL|SHIFT",  key = "DownArrow",   action = act.AdjustPaneSize { "Down",  1}},
  { mods = "CTRL|SHIFT",  key = "UpArrow",     action = act.AdjustPaneSize { "Up",    1}},
  { mods = "CTRL|SHIFT",  key = "RightArrow",  action = act.AdjustPaneSize { "Right", 1}},
}

local key_tables = BasicTable:_new {
  copy_mode = BasicTable:_new {
    -- Exit
    { mods = "NONE",   key = "q",  action = exit_copy_mode },
    { mods = "CTRL" ,  key = "g",  action = exit_copy_mode },

    -- Cursor movement
    { mods = "NONE",  key = "h",  action = act.CopyMode "MoveLeft" },
    { mods = "NONE",  key = "j",  action = act.CopyMode "MoveDown" },
    { mods = "NONE",  key = "k",  action = act.CopyMode "MoveUp" },
    { mods = "NONE",  key = "l",  action = act.CopyMode "MoveRight" },
    { mods = "NONE",  key = "w",  action = act.CopyMode "MoveForwardWord" },
    { mods = "NONE",  key = "e",  action = act.CopyMode "MoveForwardWordEnd" },
    { mods = "NONE",  key = "b",  action = act.CopyMode "MoveBackwardWord" },
    { mods = "NONE",  key = "0",  action = act.CopyMode "MoveToStartOfLine" },
    { mods = "NONE",  key = "_",  action = act.CopyMode "MoveToStartOfLineContent" },
    { mods = "NONE",  key = "$",  action = act.CopyMode "MoveToEndOfLineContent" },
    { mods = "NONE",  key = "g",  action = act.CopyMode "MoveToScrollbackTop" },
    { mods = "NONE",  key = "G",  action = act.CopyMode "MoveToScrollbackBottom" },
    { mods = "NONE",  key = "f",  action = act.CopyMode { JumpForward = { prev_char = false }}},
    { mods = "NONE",  key = "F",  action = act.CopyMode { JumpBackward = { prev_char = false }}},
    { mods = "NONE",  key = "t",  action = act.CopyMode { JumpForward = { prev_char = true }}},
    { mods = "SHIFT", key = "T",  action = act.CopyMode { JumpBackward = { prev_char = true }}},

    -- Scrolling
    { mods = "CTRL",  key = "b",  action = act.CopyMode "PageUp" },
    { mods = "CTRL",  key = "f",  action = act.CopyMode "PageDown" },
    { mods = "CTRL",  key = "y",  action = act.CopyMode "MoveToViewportTop" },
    { mods = "CTRL",  key = "e",  action = act.CopyMode "MoveToViewportBottom" },
    { mods = "CTRL",  key = "l",  action = act.CopyMode "MoveToViewportMiddle" },

    -- Selection
    { mods = "NONE" ,  key = "v",       action = act.CopyMode { SetSelectionMode = "Cell" }},
    { mods = "CTRL" ,  key = "v",       action = act.CopyMode { SetSelectionMode = "Block" }},
    { mods = "SHIFT",  key = "V",       action = act.CopyMode { SetSelectionMode = "Line" }},
    { mods = "NONE" ,  key = "y",       action = act.CopyTo "ClipboardAndPrimarySelection" },
    { mods = "NONE" ,  key = "u",       action = act.CopyMode "ClearSelectionMode" },
    { mods = "NONE" ,  key = "Escape",  action = act.CopyMode "ClearSelectionMode" },

    -- Searching
    { mods = "NONE",   key = "/",  action = act.Search "CurrentSelectionOrEmptyString" },
    { mods = "CTRL",   key = "n",  action = act.CopyMode "EditPattern" },
    { mods = "SHIFT",  key = "#",  action = act.CopyMode "ClearPattern" },
    { mods = "NONE",   key = "n",  action = act.CopyMode "NextMatch" },
    { mods = "SHIFT",  key = "N",  action = act.CopyMode "PriorMatch" },
    { mods = "NONE",   key = ",",  action = act.CopyMode "JumpReverse" },
    { mods = "NONE",   key = ";",  action = act.CopyMode "JumpAgain" },
  },

  search_mode = BasicTable:_new {
    -- Switch back to the CopyMode when the search pattern is accepted or canceled
    { mods = "NONE",  key = "Enter",   action = act.ActivateCopyMode },
    { mods = "NONE",  key = "Escape",  action = act.ActivateCopyMode },
    { mods = "CTRL",  key = "g",       action = act.ActivateCopyMode },
    -- Emacs style
    { mods = "CTRL",  key = "i",  action = act.CopyMode "CycleMatchType" },
    { mods = "CTRL",  key = "s",  action = act.CopyMode "NextMatch" },
    { mods = "CTRL",  key = "r",  action = act.CopyMode "PriorMatch" },
    { mods = "CTRL",  key = "k",  action = act.CopyMode "ClearPattern" },
  },
}

--------------------------------------------------------------------------------
--- Launch menu
--------------------------------------------------------------------------------
local launch_menu = BasicTable:_new {
  {
    label = "Bash",
    args = { "bash", "-i" },
    domain = { DomainName = "local" },
  },
  {
    label = "Fish",
    args = { "fish", "-i" },
    domain = { DomainName = "local" },
  },
  {
    label = "Pwsh",
    args = { "pwsh", "-NoLogo" },
    domain = { DomainName = "local" },
  },
}

--------------------------------------------------------------------------------
--- Domains
--------------------------------------------------------------------------------
local unix_domains = BasicTable:_new {}
local ssh_domains = BasicTable:_new(wezterm.default_ssh_domains())
local wsl_domains = BasicTable:_new(wezterm.default_wsl_domains())

--------------------------------------------------------------------------------
--- Config table
--------------------------------------------------------------------------------
local config = BasicTable:_new {
  -- Defaults
  term = "xterm-256color",
  default_prog = { "sh", "-c", "command -v fish >/dev/null && exec fish -i || exec bash -i" },

  -- System
  check_for_updates = false,
  automatically_reload_config = false,
  adjust_window_size_when_changing_font_size = false,
  audible_bell = "Disabled",
  -- Sometimes exiting SSH doesn't return 0 and it's annoying to manually close the window
  exit_behavior = "Close",
  window_close_confirmation = 'AlwaysPrompt',
  hide_mouse_cursor_when_typing = true,
  enable_wayland = false,
  front_end = "WebGpu",
  webgpu_power_preference = "LowPower",
  webgpu_force_fallback_adapter = false,
  use_ime = true,
  pane_focus_follows_mouse = false,

  -- Appearance
  initial_cols = 100,
  initial_rows = 20,
  window_decorations = "INTEGRATED_BUTTONS | RESIZE",
  enable_tab_bar = true,
  use_fancy_tab_bar = true,
  hide_tab_bar_if_only_one_tab = false,
  tab_bar_at_bottom = false,
  tab_max_width = 16,
  enable_scroll_bar = true,
  min_scroll_bar_height = "1cell",
  window_padding = {
    left = "0.5cell",
    right = "0.5cell",
    top = "0.5cell",
    bottom = "0.5cell",
  },

  -- Visual
  font = wezterm.font("Iosevka", { weight = "Regular", italic = false }),
  font_size = 12,
  -- Overwrites `color_scheme'
  colors = util.custom_color_scheme("Galaxy"),
  cursor_blink_rate = 0,
  default_cursor_style = "SteadyBlock",
  window_background_opacity = 0.9,
  text_background_opacity = 1.0,

  -- Use my own keybindings
  disable_default_key_bindings = true,
  disable_default_mouse_bindings = false,
  key_map_preference = "Mapped",
  keys = keys,
  key_tables = key_tables,

  -- Misc
  launch_menu = launch_menu,
  wsl_domains = wsl_domains,
  ssh_domains = ssh_domains,
  unix_domains = unix_domains,
}

--------------------------------------------------------------------------------
--- Overlay
--------------------------------------------------------------------------------
local ok, m = pcall(require, "wezterm-override")
if ok then
  -- The custom config has two ways to put its customizations.
  -- 1. Returns a plain table of configurations which will be merged with this
  --    config eventually.
  -- 2. Returns a table that has a function "override" which takes this config
  --    as its argument and returns a final config table (other attributes in
  --    the table will be ignored if the override function presents).
  return m["override"] ~= nil and m.override(config) or config:_imerge(m)
end
return config
