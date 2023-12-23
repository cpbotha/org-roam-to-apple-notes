-- example showing how to use contents of file as body of new note
-- this works, but the embedded base64 images almost never do
-- (I got it working once, with exactly the same settings. AppleScript for Notes is super flaky.)

set BODY_FN to (the posix path of "/tmp/sigmoid_loss_for_language_image_pre_training.html")
set NBODY to read BODY_FN
tell application "Notes"
	activate
	tell folder "org-roam"
        set newNote to make new note with properties {body:NBODY}
	end tell
end tell
