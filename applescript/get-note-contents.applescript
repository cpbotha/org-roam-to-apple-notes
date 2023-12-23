tell application "Notes"
	activate
	tell folder "orgmode"
        -- inline PDF attachment is excluded from returned HTML
        -- inline image is img src="data:image/png;base64,..."/>
        set b to get body of note "test image"
        return b
    end tell
end tell