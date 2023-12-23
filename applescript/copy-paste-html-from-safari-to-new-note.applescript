-- idea here was to load html in safari, then copy and paste rendered into new note
-- safari part works albeit very slowly, and then paste into notes is difficult because Apple

set BODY_FN to (POSIX FILE "/tmp/sigmoid_loss_for_language_image_pre_training.html")
-- load html file in safari
tell application "Safari"
    open BODY_FN
    activate
    -- now copy the rendered html to the clipboard
    tell application "System Events"
        keystroke "a" using {command down}
        keystroke "c" using {command down}
    end tell
end tell

tell application "Notes"
	activate
	tell folder "org-roam"
        set newNote to make new note with properties {body:""}
        show newNote
        delay 0.25
        -- aaarrrrgghhhhh apple why why why?!
        -- when you create a new note, there is no straight-forward way to set user-focus on the content
        -- see https://forum.keyboardmaestro.com/t/quickly-make-an-apple-note-with-note-on-top/23476/14
        -- only option is to use super fragile navigation of UI hierarchy WTaF apple
        -- in short: the following lines fail, because the body is not focused
        tell newNote
            activate
            delay 0.25
            tell application "System Events"
                keystroke "v" using {command down}
            end tell
        end tell
	end tell
end tell
