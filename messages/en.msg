CommandSignup: Sign up for duty
CommandList: List my appointments
CommandSubscribe: Subscribe to notifications
CommandUnsubscribe: Unsubscribe from notifications
CommandDelete: Delete all information about me

CommandSetOpenDays: Select days for which the volunteers are able to sign up
CommandLock: Lock the schedule and display the publishable version

Greet: Hello! This is a bot for eco-volunteers to sign up for duty in one of *parki ar minda* garages in Tbilisi.\nIf you want to learn more about us and what we do, check out @parkiarminda.\nYour application to become a volunteer is currently being evaluated by administrators. We will notify you when it is granted.
Volunteer: Congratulations, you are now a volunteer. You can now /subscribe to notifications about scheduling starts and /signup for duty.
Subscribed: You have been successfully subscribed to new notifications. Send /unsubscribe to unsubscribe.
Unsubscribed: You have been unsubscribed from new notifications. If you want to subscribe again, send /subscribe .
AreYouSure: ⚠ Are you sure? This will delete all information about you, including all the time slots you're currently allocated to. Type in `Yes, I am sure` to continue.
IAmSure: Yes, I am sure
Deleted: Your information has been successfully deleted. Press /start if you wish to start volunteering again.
NotDeleting: You did not type `Yes, I am sure`. Not deleting the chat.
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
SlotDescription where@Text when@Text: __Where__: #{where}\n__When__: #{when}
SlotFullDescription who@Text where@Text when@Text: __Who__: #{who}\n__Where__: #{where}\n__When__: #{when}
DayUnavailable: Sorry, this day is no longer available for signup, as the schedule is already set. Try another day.
NoSlots: Seems you have no appointments in the future. Consider signing up for one with /signup
YesCreate: Yes, create
Done: Done
Created: Your time slot has been *created*.\nPlease, don't forget to come, and if you can't, press the button below.\n*We will ask you to confirm the slot the day before.*
CantCome: I can't come, cancel the appointment
SureCancel: Are you sure you can't come in? This will *cancel* this appointment:
YesCancel: Yes, I can't come in, cancel
NoIWillCome: Never mind, I will come in
YourSlotCancelled: Your slot has been *cancelled*
ConfirmTomorrow: You are scheduled to be on duty tomorrow. Please confirm that you will come. If you fail to confirm this slot no later than *two hours* before the start of the slot, it will be *cancelled*:
YesIWillCome: I will come
ConfirmReminder: Looks like you still haven't confirmed your time slot for tomorrow. Please do so no later than *two hours* before the start of the slot using the confirm button in the message above.
ConfirmedShort: Your time slot has been *confirmed*.
LastReminder: Friendly reminder: you have a slot coming up in *1 hour*.
OtherDuties: It looks like you have other duties at this time. Please, choose another time or day, or cancel them.
Conflicts: Here are the conflicting duties:
ChangeGarage: Change the garage
ChangeDay: Change the day
ChangeStartTime: Change the start time
AnotherSlot: Do you want to sign up for another slot?
AnotherGarage: Sign up for another garage
SameGarage: Sign up for the same garage
CancelCreate: Cancel the other duties and create this one

Locked garageName@Text: The schedule for #{garageName} has been locked, meaning no new signups will happen. You can press the "Unlock" button to unlock the schedule.
Unlock: Unlock
Unlocked garageName@Text: The schedule for #{garageName} has been unlocked. Use /lock to lock it back and generate the final schedule.
Lock: Generate a new schedule
GarageOpenDays garage@Text: Which day should the #{garage} garage be open?
WorkingScheduleFor garage@Text: __Working schedule for #{garage}__:
NewScheduleFor garage@Text: A new schedule for #{garage}! You can use /signup or click one of the buttons below to sign up for duty.
SlotCancelled: This time slot has been cancelled. This means that the schedule which was previously generated is incorrect.
SlotStillNotConfirmed: Volunteer still has not confirmed this time slot:
SlotCancel: Cancel this slot
SetOpenDaysReminder start@Text: It's time to set the open days for garages for the next week \(starting #{start}\)!
SetOpenDays garageName@Text: Set the #{garageName} open days
VolunteerRequest user@Text: #{user} wants to become a volunteer
Allow: Allow
Decline: Decline
NewVolunteer user@Text: #{user} has been added to the list of volunteers
Ban: Ban user
VolunteerRemoved user@Text: #{user} has been removed from the list of volunteers

SlotFinished: Looks like your duty is *coming to an end*. Thank you for your help! Hopefully, it went well.

Checklist: *Please, make sure to complete all the post-duty checklist items*:

CheckBags: Tie bags if appropriate, place them in the right locations;
TakeTrashOut: Take out non-recycleable waste, make sure the garage is *clean*;
TurnOffLight: Turn *off* the lights;
CloseTheDoor: Close and *lock* the door;
ReturnKey: Return the *key* to the proper location;
ReportVisitors: Report how many visitors there were, by *replying to this message* with just a number \(like this: `42`\) and nothing else \(*not* like this: `there were 42.`\).
Number: Number
Ack: Acknowledged, thanks!

ScheduleIntro garageName@Text garageAddress@Text: Schedule for #{garageName} garage \(#{garageAddress}\):
ScheduleTime from@Text to@Text:⏰ #{from} - #{to}\n

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

Date month@Text day@Text dayOfWeek@Text: #{dayOfWeek}, #{month} #{day}