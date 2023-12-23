-- On Sonoma 14.2, the best you can get is a duplicated PDF attachment in the note
-- See comments below in code

set TITLE to "Hello dere AA"
set NBODY to "<h1>Hello dere AA</h1> bleh bleh 22 33"
tell application "Notes"
	activate
	tell folder "orgmode"
		if not (note named TITLE exists) then
			set newNote to make new note with properties {name:TITLE}
			-- display dialog (aid as text)
			-- this body setting with its heading will overwrite the unformatted note title
			set body of newNote to NBODY
			-- somehow, this makes the same PDF appear twice in the note
			-- why??
			-- if I set the body after the make new attachment, then NO PDFs are listed
			-- I am not the only one:
			-- https://www.macscripter.net/t/how-can-i-attach-a-pdf-to-a-note-in-notes-app/73575/17
			make new attachment at end of attachments of newNote with data (POSIX file "/Users/charlbotha/Library/Mobile Documents/com~apple~CloudDocs/orgmode test/3446194.pdf")
			-- following experiment also did not work
			-- make new attachment of newNote with data (POSIX file "/Users/charlbotha/Library/Mobile Documents/com~apple~CloudDocs/orgmode test/3446194.pdf")
			-- delete first attachment of newNote
		else
			set existingNote to note named TITLE
			set body of existingNote to "here we are again"
		end if
	end tell
end tell
