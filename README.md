# LimeControl2: Track and Manage Survey Solicitations

## Intended Use

LimeControl2 is designed to streamline and monitor telephone survey solicitations for LimeSurvey surveys.

## Main Features

+ **Survey progress tracking:** The app offers detailed information about who responded to the survey and when. It also monitors invitations and reminders.
+ **Multiple Survey Support:** The app can support several surveys simultaneously. You can switch between active surveys using a dropdown menu.

![UI](screenshots/tracking.png?raw=true "Survey tracking interface")

+ **Reminders:** A key feature is the ability to send reminders to individual respondents. Following up a phone conversation with an email can significantly boost the response rate.
+ **Note taking.** After contacting a study participant, you can take notes and update their email addresses. All this data is stored in your survey's participants table.
+ **Excel Export.** You can export your data to Excel at any time. This data is also accessible via LimeSurvey.

### User Management

LC2 provdies a simple login module. Currently, the module doesn't support password hashing. I have no formal background in IT, nor am I a cyber-security expert. You are using the provided module at your own risk. If possible, you should use professional tools for app deployment and user authorization such as Posit Connect.

You cannot create temporary or anonymous users with LimeSurvey API. This means that if the app is used by several users concurrently each of these users is sending calls to Lime API and each needs an individual Lime account. LC2 assumes that all authorization credentials will be relayed as environment variables.

### Technical Limitations

For simplicity's sake, all data entered into the app is stored in the corresponding LimeSurvey participants table. Since LimeSurvey does not permit users to add new columns to the participants table via API, you'll need to manually set up the necessary columns beforehand. Then, adjust the application code to match. ¯\\\_(ツ)_/¯