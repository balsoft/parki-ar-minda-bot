# Telegram bot for parki ar minda volunteers

This is a telegram bot which allows volunteers for [parki ar minda](https://parkiarminda.ge) to sign up for duties in garages.

It's written in Haskell (because it's fun(ctional)) and uses sqlite as the database.

## Building

The easiest way to build this is with Nix.

<details>
  <summary>If you know Nix</summary>
  <code>
    nix build github:balsoft/parki-ar-minda-bot
  </code>
</details>

1. [Get Nix](https://nixos.org/download/) if not already installed
2. Enable flakes: `mkdir -p ~/.config/nix && echo 'experimental-features = nix-command flakes' >> ~/.config/nix/nix.conf` if not already enabled
3. Clone the repo: `git clone https://github.com/balsoft/parki-ar-minda-bot && cd parki-ar-minda-bot`
4. Build:
    * Just build it: `nix build`; the binary will be in `./result/bin/parki-ar-minda-bot`
    * **OR** get a shell from which you can run it: `nix shell`
    * **OR** build a static binary that can run without Nix on **any Linux system**: `nix build .#parki-ar-minda-bot-static`; the binary will be in `./result/bin/parki-ar-minda-bot`
5. Deploy: copy the static executable from previous step to any Linux box & find a way to launch it (systemd, initscript, etc).

## Running

_One of these days I'll write a `--help` message..._

The executable does not accept any CLI flags; instead, all configuration is done through environment variables.

These are **REQUIRED**:

|Variable name|Description|Example|
|:-:|:-:|:-:|
|`PARKI_AR_MINDA_TELEGRAM_TOKEN`|[Telegram token] for the bot account; get it through [BotFather]. Be careful not to leak it as it allows people to authenticate as your bot.|`123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11`|

[Telegram token]: https://core.telegram.org/bots/api#authorizing-your-bot
[BotFather]: https://core.telegram.org/bots/features#botfather

There are _OPTIONAL_:

|Variable name|Default value|Description|Example|
|:-:|:-:|:-:|:-:|
|`PARKI_AR_MINDA_DATABASE`|`database.sqlite`|A path (absolute or relative to PWD) to an sqlite3 database. The database will be created if it does not exist. Migrations are applied to the database automatically. You should back up the database periodically and before updates, in case things go wrong|`./database2.sqlite`|
|`PARKI_AR_MINDA_REQUESTS_TIME`|`12:00:00`|A time at which requests to confirm the slot are sent (on the day before the slot)|`13:00:00`|
|`PARKI_AR_MINDA_REMINDERS_TIME`|`18:00:00`|A time at which reminders to confirm the slot and warnings that the slot isn't confirmed are sent (on the day before the slot)|`19:00:00`|
|`PARKI_AR_MINDA_OPEN_DAY_REMINDERS_DAY`|`Wednesday`|The day of week on which reminders to open the next week for signups are sent to admins|`Thursday`|
|`PARKI_AR_MINDA_OPEN_DAY_REMINDERS_TIME`|`12:00:00`|The time at which reminders to open the next week for signups are sent to admins (on `PARKI_AR_MINDA_OPEN_REMINDERS_DAY`)|`13:00:00`|
|`PARKI_AR_MINDA_APP_URL`|_None_|URL to the parki ar minda app for the integration. Unless both this variable and `PARKI_AR_MINDA_APP_TOKEN` are present, the integration is disabled. If the integration is enabled, it automatically sends schedule information to the app|`./database2.sqlite`|
|`PARKI_AR_MINDA_APP_TOKEN`|_None_|Token to authorize with the parki ar minda app for the integration. Unless both this variable and `PARKI_AR_MINDA_APP_URL` are present, the integration is disabled|`./database2.sqlite`|
|`PARKI_AR_MINDA_ICS_DIRECTORY`|_None_|The directory in which to save ics (iCalendar) files whenever a schedule is locked. If set, the directory must exist and be writable by the bot before the bot is started. If not set, saving ics files is disabled.|`/var/www/calendars`|

### Required database hackery

Currently there's no way to add or remove bot admins from the bot itself on startup. After starting the bot and sending `/start` to it, you have to manually edit the database.

This is also useful if you accidentally lock yourself out for some reason.

To add an admin, we must:

1. Find the desired user id we want to add by looking in the `telegram_user` table (id is the first field)
2. Find the admin id of the last admin in the `admin` table (id is the first field)
3. Insert a new admin as (last_admin_id + 1, user_id)

To remove an admin, we must:

1. Find the admin's user id in the `telegram_user` table (id is the first field)
2. Remove the admin by their user id

Here is an example:

```
$ sqlite3 database.sqlite
SQLite version 3.48.0 2025-01-14 11:05:00
Enter ".help" for usage hints.
sqlite> SELECT * FROM telegram_user ;
1|1234|en|Alice|alice
2|3456|ru|Bob|bob
sqlite> SELECT * FROM admin;
1|2
sqlite> INSERT INTO admin VALUES (2,1);
sqlite> SELECT * FROM admin;
1|2
2|1
sqlite> DELETE FROM admin WHERE user = 2;
sqlite> SELECT * FROM admin;
2|1
```

In the beginning, Bob (user id 2) is an admin and Alice is not.

Bob's admin id is 1 (note that it is unrelated to Bob's user id).

We add Alice (user id 1) as an admin, with admin id 2.

Then we remove Bob (user id 2) as the admin.

In the end, Alice (user id 1) is an admin and Bob is not.

## Usage

Bot's commands are available in the "Menu" (button in the bottom left of the screen) in most telegram clients.

### Admin workflow

Admins can (and must) do multiple things. A good place to start is adding a garage.

Send a `/newgarage` command to the bot.

The bot will then ask you for Name, Address and Link. Reply with text messages to set those values to the contents of your replies.

All parts of that (Name, Address, Link) are really just text fields that you can set to whatever text you want, but it's best to keep the name simple, the address short, and the link, well, a URL.

In the end you will be shown details about the garage.

You can edit the garage by clicking the corresponding button under the garage info. It is very similar to adding a garage, except the bot suggests you the previously set values for all those fields.

You can also disable the garage (but not delete it, to prevent accidentally deleting years of statistics).

Disabled garages disappear from the rest of the app.

To show the list of all garages, use `/garages`.

Once you've added all the garages you want, you can set the "open days" for them.

Open days are days during which the garage is open, i.e. during which volunteers can sign up for duties.

Send `/setopendays` to the bot and it sends you a message for each garage asking you to set the days. Click on days during which the garage should be open (checkmarks appear to confirm your selection) and then click "Done". Previous open days for every garage are saved as the default for the next week, so typically you can just click "Done".

After you set the open days for a garage, all admins will be sent a "working schedule" message, where all duties for that garage and for that week are displayed.

Another message is be sent to all subscribed volunteers about the new schedule.

Volunteers are now able to sign up for duties in that garage during all open days.

Admins are able to Edit the working schedule (which just means setting different open days for the garage) or Lock it (which means no new sign-ups can happen).

When an admin locks a schedule, volunteers are no longer able to sign up for duties, the "locked" schedule appears in the same message as the working schedule. In it, all slots are merged together and volunteer names are removed for privacy. This can be then shared with the garage visitors.
There are also messages in different formats sent to admins, for importing into other systems.

Admins can also lock next-week schedules for all garages simultaneously by sending `/lock`.

If a volunteer doesn't confirm a slot in time, a warning will be sent with the details. Admins can cancel that slot.

Whenever an admin or a volunteer cancels a slot, a different warning is sent.
This means that either someone has to substitute (by clicking on the link provided in the message) or the admins have to notify visitors about a change in schedule.
Both the locked and the working schedule in the working schedule message will be up-to-date, but to get other formats you can unlock it and then re-lock it.

Whenever a new user tries to talk with the bot, admins are notified. They can add the new user as a volunteer, and if they later decide to remove the user there's a ban button.

`/volunteers` lists all volunteers, with the ability to ban them.

`/newadmin @username` will add `@username` as an admin, with the ability to remove them. They must have talked with the bot already.

Finally, to get statistics about duties from the bot, you can send `/report`. You can limit the time for which the report is generated by adding either one or two dates, in [ISO 8601] (with days and/or months omitted), separated by spaces. Examples:

- `/report` will produce a report with all slots
- `/report 2025` will produce a report with all slots since the beginning of 2025 until now. This allows to get statistics since the beginning of the year.
- `/report 2024 2025` will produce a report with all slots that happened during 2024 (so, since 2024-01-01 until 2024-12-31 inclusively). It allows to get statistics for an entire year in the past.
- `/report 2024-04 2025-02` will produce a report with slots between 1st of April 2024 and 1st of February 2025.
- `/report 2025-06` will produce a report with slots between 1st of June 2025 and this day. This allows to get all slots for the current month.
- `/report 2024-04-08 2025-02-18` will produce a report with slots between 8th of April 2024 and 18th of February 2025. This can allow for some very granular filtering but is probably overkill :)

If no slots are returned, you don't get anything ~~(because telegram throws an exception and I'm too lazy to fix it)~~.

[ISO 8601]: https://en.wikipedia.org/wiki/ISO_8601

### Volunteer 

You can subscribe to messages about open days in all garages by sending `/subscribe` and unsubscribe with `/unsubscribe`.

If you want to sign up for a duty, the most convenient way is with `/signup`. It presents a multiple-step menu which can be navigated purely with button presses.

You can (almost) always come back to any such menu, even if there are newer messages between yourself and the bot.

Volunteers who signed up for a duty will get a request to confirm it, and then another reminder. If they don't confirm the slot in time, a warning is sent to admins.

You can list the duties you have with `/list`.

If you can't attend a duty, you should cancel it by pressing the corresponding button and confirming the cancellation.
