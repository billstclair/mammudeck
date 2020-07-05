# Mammudeck TODO

Mammudeck now has like, repeat, and a bare-bones post dialog. It's usable for day-to-day Fediverse interaction. But there's a lot more to do.

Bill St. Clair, 2 July 2020

## For the Post dialog

* Reply should @mention the participants.
    DONE, except favorite and repost notifications, plus
    reposted timeline entries should include those people as well.
    Do this by adding the including notification's or status's 
    mentions to the included status.
    Or not. Or a preference. Or buttons to choose.
* Don't @mention yourself in a reply.
* Images in posts (and fix sensitivity and spoiler text in the client library)
    DONE, with drag-and-drop.
* Show replied/quoted post in the post dialog, likely optional.
* Reply and Quote checkboxes, so you can change if/how the post mentions another
* Visibility (non, public, unlisted, private, direct)
* Post size tracking and limitation
* Editing of posts
    (actual edit on servers that support that, delete/repost on others)
* Upload videos.

## For the Columns view

* Server switcher in the left column.
    Change the master server.
    Will be less important once a feedset can fetch from multiple servers.
* "Update feed" button at top of feed (includes the merge code needed
    for auto-update).
* Auto-update, likely without WebSockets initially
    * Indicators of how many posts have been loaded but not displayed
      Click to add to top of column
    * Indicators of how many invisible posts are off screen, to right and left
      Click to scroll to the next one in that direction

## For column status rendering

* Show "in reply to" "user"
    Pop up replied-to post on hover over "in reply to"
    Pop up user profile on hover over user
* Show quoted post inline, with hide/show
* Ellipsis dialog: block, mute, (un)follow or delete, edit
* Replace :<emoji>: with the URL from the "GET custom_emojis" API request.
* Image view popup (currently, clicking on image opens the full-res version in a new tab)
* Display Polls
* Video attachments.

## Thread Explorer

* Navigate the `Context` entities returned by `GET statuses/:id/context`.
* The explorer will pop up when you click on the timestamp of a status.
* If the status has non-null `in_reply_to_id` non-zero
    `replies_count`, then this the explorer will fetch context,
    and create a twist-down tree explorer for investigating it.
    
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

* DONE modulo the Progressive Web App manifest, so saving to desktop will
    get a better icon and name.
    https://developer.mozilla.org/en-US/docs/Web/Progressive_web_apps
    https://web.dev/progressive-web-apps/

## New Settings dialog

* Column width, possibly per-column (photo posters elicit more real estate)
    DONE, but not per-column
* Font size
    DONE.
* Color options other than light/dark
    Do everything with CSS classes, not inline styles.
    User editing of the CSS classes

And that's just to get to version 1.0. Bill be busy boy.
