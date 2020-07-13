# Mammudeck TODO

Mammudeck now has like, repeat, and a bare-bones post dialog. It's usable for day-to-day Fediverse interaction. But there's a lot more to do.

Bill St. Clair, 2 July 2020

## For the Post dialog

* Show replied/quoted post in the post dialog, likely optional.
* Reply and Quote checkboxes, so you can change if/how the post mentions another.
* Post to a group.
* Visibility (non, public, unlisted, private, direct)
* Post size tracking (let them post and get an error if too big).
* Editing of posts
  (actual edit on servers that support that, delete/repost on others).
* Upload videos.
* Reply should @mention the participants.
  **DONE**, except favorite and repost notifications, plus
  reposted timeline entries should include those people as well.
  Do this by adding the including notification's or status's 
  mentions to the included status.
  Or not. Or a preference. Or buttons to choose.
* See if there's any way to convert a data URL back into a File,
  so that we can recover from the browser navigating away while a
  file is being uploaded.
* Don't @mention yourself in a reply.
  **DONE**.
* Images in posts (and fix sensitivity and spoiler text in the client library).
  **DONE**, with drag-and-drop.

## For the Columns view

* "Update feed" button at top of feed (includes the merge code needed
  for auto-update).
* Auto-update, likely without WebSockets initially
  * Indicators of how many posts have been loaded but not displayed
    Click to add to top of column
  * Indicators of how many invisible posts are off screen, to right and left
    Click to scroll to the next one in that direction
* Server switcher in the left column.
  **DONE**.
  Change the master server.
  Will be less important once a feedset can fetch from multiple servers.
* It is possible to scroll to the end of a feed.
  Detect this, and don't keep loading over and over to get nothing.
* The group columns, with higher headers, are rendering past the bottom
  of the window.

## For column status rendering

* Turn foo.com into https://foo.com in the client, since the Gab backend
  doesn't do that. It apparently does it in the client, too (but check that).
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
  Groups mostly done, except:
  * Incremental search for group name in "Edit Columns" dialog,
    instead of entering ID.
* Popup interactive search for users
* Popup interactive search for hashtags
* Popup interactive search for groups
* Feeds from other servers
  Fetch with logged-in token, if we have it, or unlogged-in otherwise
* Multiple feedsets per logged-in host.
  Switcher in left column
* Save/Restore feedsets as a JSON string.

## Mobile layout!

* **DONE** modulo the Progressive Web App manifest, so saving to desktop will
  get a better icon and name.
  * https://developer.mozilla.org/en-US/docs/Web/Progressive_web_apps
  * https://web.dev/progressive-web-apps/
  * **ALL DONE**

## User dialog

* Popup whenever you click on an @user mention, or the column header of a user feed.
* Show all the stuff on Gab.com's profile page.
* Link to `Account.url`.
* Add or remove column.

## Group dialog

* Popup whenever you click on a group name, or on the member count in a
  group feed header.
* Show `Group.title`, `Group.description`, `Group.cover_image_url`, `Group.member_count`.
* Enable joining or leaving the group.
* Link to `groupUrl renderEnv group`.
* Add or remove column.
* Adiminstrative stuff.

## List dialog

* Show `ListEntity.title`, and some of the info from `ListsRequest <| GetListAccounts { id }`
* Add and remove accounts.
* Rename the list.

## Settings dialog

* Make it an option to replace the left column with a floating button.
  That buton will bring up a "Post" dialog, with a button to switch to
  the settings dialog
* Color options other than light/dark
  Do everything with CSS classes, not inline styles.
  User editing of the CSS classes
* Reprobe for features.
* Column width, possibly per-column (photo posters elicit more real estate).
  **DONE**, but not per-column.
* Font size.
  **DONE**

And that's just to get to version 1.0. Bill be busy boy.
