# Mammudeck TODO

Mammudeck now has like, repeat, and a bare-bones post dialog. It's usable for day-to-day Fediverse interaction. But there's a lot more to do.

Bill St. Clair, 2 July 2020

## For the Post dialog

* Reply should @mention the participants.
    Done, except favorite and repost notifications, plus
    reposted timeline entries should include those people as well.
    Do this by adding the including notification's or status's 
    mentions to the included status.
    Or not.
* Images in posts (and fix sensitivity and spoiler text in the client library)
    Done modulo drag-and-drop.
* Show replied/quoted post in the post dialog, likely optional.
* Reply and Quote checkboxes, so you can change if/how the post mentions another
* Visibility (non, public, unlisted, private, direct)
* Post size tracking and limitation
* Editing of posts
    (actual edit on servers that support that, delete/repost on others)

## For the Columns view

* Auto-update, likely without WebSockets initially
    * Indicators of how many posts have been loaded but not displayed
      Click to add to top of column
    * Indicators of how many invisible posts are off screen, to right and left
      Click to scroll to the next one in that direction

## For column status rendering

* Replace :<emoji>: with the URL from the "GET custom_emojis" API request.
* Show "in reply to" "user"
    Pop up replied-to post on hover over "in reply to"
    Pop up user profile on hover over user
* Show quoted post inline, with hide/show
* Image view popup (currently, clicking on image opens the full-res version in a new tab)

## For the Feed set "edit" dialog

* The rest of the feed types (hashtag, list, group, conversations, search)
* Popup interactive search for users
* Popup interactive search for hashtags
* Popup interactive search for groups
* Feeds from other servers
    Fetch with logged-in token, if we have it, or unlogged-in otherwise
* Multiple feedsets per logged-in host.
    Switcher in left column
* Save/Restore feedsets as a JSON string.

## Mobile layout!

## New Settings dialog

* Column width, possibly per-column (photo posters elicit more real estate)
* Font size
* Color options other than light/dark
    Do everything with CSS classes, not inline styles.
    User editing of the CSS classes

And that's just to get to version 1.0. Bill be busy boy.