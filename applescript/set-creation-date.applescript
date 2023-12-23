-- with this script, i wanted to check if I could change the created / modified date of an imported note
-- to match the original 
-- It was unfortunately not possible. I'm not surprised anymore.

use framework "Foundation"
use scripting additions

set dateFormatter to current application's NSDateFormatter's new()
dateFormatter's setDateFormat:"yyyy-MM-dd'T'HH:mm:ssZ"
set isoDateString to "2022-12-23T12:00:00Z" -- Replace this with your ISO 8601 timestamp
-- in a separate script, this works fine, but inside the tell clauses below I'm getting
-- execution error: Can’t make «class ocid» id «data optr00000000A353CBB23753C38F» into type date. (-1700)
set appleScriptDate to (dateFormatter's dateFromString:isoDateString) as date

tell application "Notes"
	activate
	tell folder "org-roam"
        set newNote to make new note with properties {body:"creation date test"}

        -- date Saturday, 23 December 2023 at 17:30:49
        -- get creation date of newNote

       appleScriptDate

       -- either of these will yield:
       -- execution error: Notes got an error: Can’t set modification date of note to date "Friday, 23 December 2022 at 14:00:00". (-10006
       set creation date of newNote to appleScriptDate
       set modification date of newNote to appleScriptDate
	end tell
end tell