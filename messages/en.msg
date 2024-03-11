CommandSignup: Sign up for duty
CommandList: List my appointments
CommandSubscribe: Subscribe to notifications
CommandUnsubscribe: Unsubscribe from notifications
CommandDelete: Delete all information about me

CommandSetOpenDays: Select days for which the volunteers are able to sign up
CommandLock: Lock the schedule and display the publishable version
CommandWorkingSchedule: Show working schedule for next week
CommandWorkingScheduleThisWeek: Show working schedule for this week
CommandReport: Generate a csv report of all slots

Greet: Hello! This is a bot for eco-volunteers to sign up for duty in one of <b>parki ar minda</b> garages in Tbilisi.\nIf you want to learn more about us and what we do, check out @parkiarminda.\nYour application to become a volunteer is currently being evaluated by administrators. We will notify you when it is granted.
Volunteer: Congratulations, you are now a volunteer. You can now /subscribe to notifications about scheduling starts and /signup for duty.
Subscribed: You have been successfully subscribed to new notifications. Send /unsubscribe to unsubscribe.
Unsubscribed: You have been unsubscribed from new notifications. If you want to subscribe again, send /subscribe .
AreYouSure: Are you sure? This will delete all information about you, including all the time slots you're currently allocated to. Type in <code>Yes, I am sure</code> to continue.
IAmSure: Yes, I am sure
Deleted: Your information has been successfully deleted. Press /start if you wish to start volunteering again.
NotDeleting: You did not type <code>Yes, I am sure</code>. Not deleting the chat.
SomethingWentWrong: Sorry, something went wrong :/
NotRecognized: Sorry, I don't understand this message. Maybe you lack the permissions to do this?
Signup: Thank you for your interest! You can sign up for multiple duty slots using the menu below. If you change your mind or don't want to sign up to any more slots, press the Cancel button.
ChooseGarage: Please choose a garage
ChooseDay: Please choose a day
NoDays: Looks like there are no open days scheduled for this garage. Try another garage.
OtherVolunteers: There are some other volunteers signed up for that day already.
ChooseExistingSlot: You can join them or propose your own time slot.
OtherDutiesThisDay: Note that you already have some other duties this day:
NewAppointment: I'd like to propose my own time slot
ChooseStartTime: Please choose the start of your available time slot
ChooseEndTime: Please choose the end of your available time slot
Cancel: Cancel
Create: Do you want to create this appointment?
Where: Where
When: When
Who: Who
DayUnavailable: Sorry, this day is no longer available for signup, as the schedule is already set. Try another day.
NoSlots: Seems you have no appointments in the future. Consider signing up for one with /signup
YesCreate: Yes, create
Done: Done
Created: Your time slot has been <b>created</b>.\nPlease, don't forget to come, and if you can't, press the button below.\n<b>We will ask you to confirm the slot the day before.</b>
CantCome: I can't come, cancel the appointment
SureCancel: Are you sure you can't come in? This will <b>cancel</b> this appointment:
YesCancel: Yes, I can't come in, cancel
NoIWillCome: Never mind, I will come in
YourSlotCancelled: Your slot has been <b>cancelled</b>
ConfirmTomorrow: You are scheduled to be on duty tomorrow. Please confirm that you will come. If you fail to confirm this slot no later than <b>two hours</b> before the start of the slot, it will be <b>cancelled</b>:
YesIWillCome: I will come
ConfirmReminder: Looks like you still haven't confirmed your time slot for tomorrow. Please do so no later than <b>two hours</b> before the start of the slot.
ConfirmedShort: Your time slot has been <b>confirmed</b>.
LastReminder: Friendly reminder: you have a slot coming up in <b>1 hour</b>.
OtherDuties: It looks like you have other duties at this time. Please, choose another time or day, or cancel them.
Conflicts: Here are the conflicting duties:
ChangeGarage: Change the garage
ChangeDay: Change the day
ChangeStartTime: Change the start time
AnotherSlot: Do you want to sign up for another slot?
AnotherGarage: Sign up for another garage
SameGarage: Sign up for the same garage
CancelCreate: Cancel the other duties and create this one

Locked: This schedule has been locked, meaning no new signups will happen. You can press the "Unlock" button to unlock the schedule.
Unlock: Unlock
Unlocked: This schedule is unlocked. You can press the "Lock" button to lock this schedule before publishing it, or use /lock to lock all schedules for this week.
Lock: Lock
GarageOpenDays garage@Html: Which day should the #{garage} garage be open?
WorkingScheduleFor garage@Html: Working schedule for #{garage}:
NewScheduleFor garage@Html: A new schedule for #{garage}! You can use /signup or click one of the buttons below to sign up for duty.
SlotCancelled: This time slot has been cancelled. This means that the schedule which was previously generated is incorrect. Please check the new schedule.
SlotStillNotConfirmed: <b>Volunteer still has not confirmed this time slot</b>:
SlotCancel: Cancel this slot
SetOpenDaysReminder start@BotMessage: It's time to set the open days for garages for the next week (starting #{_tr "en" start})!
VolunteerRequest user@Html: #{user} wants to become a volunteer
Allow: Allow
Decline: Decline
NewVolunteer user@Html: #{user} has been added to the list of volunteers
Ban: Ban user
VolunteerRemoved user@Html: #{user} has been removed from the list of volunteers

Legend: Legend
LegendCreated: Created
LegendAwaitingConfirmation: Awaiting Confirmation
LegendUnconfirmed: Unconfirmed
LegendConfirmed: Confirmed
LegendFinished: Finished
LegendVisitors: Visitors

SlotFinished: Looks like your duty is <b>coming to an end</b>. Thank you for your help! Hopefully, it went well.

Checklist: Please, make sure to complete all the post-duty checklist items:

CheckBags: Tie bags if appropriate, place them in the right locations;
TakeTrashOut: Take out non-recycleable waste, make sure the garage is <b>clean</b>;
TurnOffLight: Turn <b>off</b> the lights;
CloseTheDoor: Close and <b>lock</b> the door;
ReturnKey: Return the <b>key</b> to the proper location;
ReportVisitors: Report how many visitors there were, by <b>replying to this message</b> with just a number (like this: <code>42</code>) and nothing else (<b>not</b> like this: <code>there were 42.</code>).
Number: Number
Ack: Acknowledged, thanks!

Reported visitors@Int: There were #{visitors} visitors during this duty:

ScheduleIntro garage@Html: Schedule for #{garage}:

Monday: Monday
Tuesday: Tuesday
Wednesday: Wednesday
Thursday: Thursday
Friday: Friday
Saturday: Saturday
Sunday: Sunday

January: January
February: February
March: March
April: April
May: May
June: June
July: July
August: August
September: September
October: October
November: November
December: December

Date month@BotMessage day@Int dayOfWeek@BotMessage: #{_tr "en" dayOfWeek}, #{_tr "en" month} #{day}
