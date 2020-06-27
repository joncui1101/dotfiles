-- HANDLE SCROLLING

local deferred = false

overrideRightMouseDown = hs.eventtap.new({ hs.eventtap.event.types.rightMouseDown }, function(e)
    deferred = true
    return true
end)

overrideRightMouseUp = hs.eventtap.new({ hs.eventtap.event.types.rightMouseUp }, function(e)
    if (deferred) then
        overrideRightMouseDown:stop()
        overrideRightMouseUp:stop()
        hs.eventtap.rightClick(e:location())
        overrideRightMouseDown:start()
        overrideRightMouseUp:start()
        return true
    end

    return false
end)


local oldmousepos = {}
local scrollmult = -4   -- negative multiplier makes mouse work like traditional scrollwheel
dragRightToScroll = hs.eventtap.new({ hs.eventtap.event.types.rightMouseDragged }, function(e)
    deferred = false

    oldmousepos = hs.mouse.getAbsolutePosition()

    local dx = e:getProperty(hs.eventtap.event.properties['mouseEventDeltaX'])
    local dy = e:getProperty(hs.eventtap.event.properties['mouseEventDeltaY'])
    local scroll = hs.eventtap.event.newScrollEvent({dx * scrollmult, dy * scrollmult},{},'pixel')

    -- put the mouse back
    hs.mouse.setAbsolutePosition(oldmousepos)

    return true, {scroll}
end)

overrideRightMouseDown:start()
overrideRightMouseUp:start()
dragRightToScroll:start()


-- Inspired by Linux alt-drag or Better Touch Tools move/resize functionality
-- from https://gist.github.com/kizzx2/e542fa74b80b7563045a
-- Command-ctrl-move: move window under mouse
-- Alt-ctrl-move: resize window under mouse
function get_window_under_mouse()
   local my_pos = hs.geometry.new(hs.mouse.getAbsolutePosition())
   local my_screen = hs.mouse.getCurrentScreen()
   return hs.fnutils.find(hs.window.orderedWindows(), function(w)
                             return my_screen == w:screen() and
                                w:isStandard() and
                                (not w:isFullScreen()) and
                                my_pos:inside(w:frame())
   end)
end

dragging = {}                   -- global variable to hold the dragging/resizing state

-- global timer to maximize the dragging window.
max_timer = hs.timer.new(0, function()
    if dragging.win then
        dragging.win:maximize(0)
    end
end)

drag_event = hs.eventtap.new({ hs.eventtap.event.types.mouseMoved }, function(e)
    if not dragging then return nil end
    if dragging.mode==3 then -- just move
        local dx = e:getProperty(hs.eventtap.event.properties.mouseEventDeltaX)
        local dy = e:getProperty(hs.eventtap.event.properties.mouseEventDeltaY)
        dragging.win:move({dx, dy}, nil, false, 0)
        hs.alert.show(hs.geometry.new(hs.mouse.getRelativePosition()).x)
        hs.alert.show(dragging.win:screen():frame().x)

        -- if the window is dragged to the top of the screen, maximize it using a timer.
        if dragging.win:frame().y == dragging.win:screen():frame().y then
            max_timer:setNextTrigger(0.5)
        else
            if max_timer:running() then
                max_timer:stop()
            end
        end
    else -- resize
        local pos=hs.mouse.getAbsolutePosition()
        local w1 = dragging.size.w + (pos.x-dragging.off.x)
        local h1 = dragging.size.h + (pos.y-dragging.off.y)
        dragging.win:setSize(w1, h1)
    end
end)

flags_event = hs.eventtap.new({ hs.eventtap.event.types.flagsChanged }, function(e)
      local flags = e:getFlags()
      local mode=(flags.ctrl and 1 or 0) + (flags.cmd and 2 or 0) + (flags.alt and 4 or 0)
      if mode==3 or mode==5 then -- valid modes
         if dragging then
            if dragging.mode == mode then return nil end -- already working
         else
            -- only update window if we hadn't started dragging/resizing already
            dragging={win = get_window_under_mouse()}
            if not dragging.win then -- no good window
               dragging=nil
               return nil
            end
         end
         dragging.mode = mode   -- 3=drag, 5=resize
         if mode==5 then
            dragging.off=hs.mouse.getAbsolutePosition()
            dragging.size=dragging.win:size()
         end
         drag_event:start()
      else                      -- not a valid mode
         if dragging then
            drag_event:stop()
            dragging = nil
         end
      end
      return nil
end)
flags_event:start()

hs.hotkey.bind({"cmd", "ctrl", "shift"}, hs.keycodes.map.up, function()
    local win = hs.window.focusedWindow()
    if win then
        win:maximize(0)
    end
end)

hs.hotkey.bind({"cmd", "ctrl", "shift"}, hs.keycodes.map.left, function()
    local win = hs.window.focusedWindow()
    if win then
        local res = win:screen():frame()
        win:setFrame({x=res.x, y=res.y, w=res.w/2, h=res.h}, 0)
    end
end)

hs.hotkey.bind({"cmd", "ctrl", "shift"}, hs.keycodes.map.right, function()
    local win = hs.window.focusedWindow()
    if win then
        local res = win:screen():frame()
        win:setFrame({x=res.x+res.w/2, y=res.y, w=res.w/2, h=res.h}, 0)
    end
end)
