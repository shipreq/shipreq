Third-Party Services
====================

## GSuite

Admin - https://admin.google.com/shipreq.com

For signed emails:
1. Add a TXT record for SPF: https://support.google.com/a/answer/178723
2. Enable DKIM
  1. Wait 24-48 hrs (blame Google)
  2. Search for "DKIM" from https://admin.google.com/shipreq.com
  3. …which leads to https://admin.google.com/shipreq.com/AdminHome?fral=1#AppDetails:service=email
  4. Genereate DKIM records
  5. Update DNS

Creating archive@shipreq.com
1. Create new group.
2. Edit group
3. Permissions > Access permissions > Contact the owners of this Group : deselect Public
4. Permissions > Posting permisson > Post : enbable public (so that forwarding rules on incoming emails works)

Creating {news,notice}@shipreq.com
1. Create new group: notice
1. Add alias: news
2. Edit group
3. Permissions > Posting permisson > Post : enbable public
4. Permissions > Posting permisson > Post As The Group : set to All
5. Permissions > Posting permisson > Reply To Author : set to Public

To redirect mail to FreshDesk:
1. Search for "default routing" from https://admin.google.com/shipreq.com
2. …which leads to https://admin.google.com/shipreq.com/AdminHome#AppDetails:service=email&flyout=default_routing
3. Click "ADD SETTING"
  1. Set 'Single recipient' to contact@shipreq.com
  2. Check 'Change envelope recipient' and enter the FreshDesk email
  3. Save.

To BCC FreshDesk ingress to archive@shipreq.com:
1. Edit the route created above.
2. Check 'Add more recipients' > Add > Advanced
3. Uncheck 'Do not deliver spam to this recipient'
3. Uncheck 'Suppress bounces from this recipient'
4. Check 'Change envelope recipient' and enter archive@shipreq.com

Configure SMTP for Taskman
1. Search for "SMTP relay service" from https://admin.google.com/shipreq.com
2. …which leads to https://admin.google.com/shipreq.com/AdminHome?fral=1&groupId=archive@shipreq.com&chromeless=1#ServiceSettings/notab=1&service=email&subtab=filters
3. Click "CONFIGURE"
  1. Enter desc: "SMTP for Taskman"
  2. Set 'Allowed senders' to 'Only addresses in my domain'
  3. Check 'Require SMTP Authentication'
  4. Check 'Require TLS encryption'
  5. Click 'ADD SETTING'
4. Search for setting: "Less secure apps" → https://admin.google.com/shipreq.com/AdminHome?fral=1#ServiceSettings/notab=1&service=securitysetting&subtab=lesssecureappsaccess
5. Select: 'Allow users to manage their access to less secure apps'
6. Enable "Allow less secure apps" for the user@shipreq.com that will login to SMTP
7. Taskman ettings:
    mail.smtp.auth                     = true
    mail.smtp.host                     = smtp-relay.gmail.com
    mail.smtp.port                     = 587
    mail.smtp.starttls.enable          = true
    mail.user                          = <user>@shipreq.com
    mail.password                      = <user's password>
↑ Problems:
1. Google security. Usage from new IP will fail, user will have to login and approve IP.
2. Google Compute Engine does not allow outbound connections on ports 25, 465, and 587.


## MailChimp

1. Account Settings -> Verified domains -> add `shipreq.com`.
2. Account Settings -> Extras -> Create API key.
3. Lists -> Create
    * Name = Master
    * From Email = contact@shipreq.com
    * From Name = ShipReq
    * Remind-how = You are receiving this email because you opted in at our website.
4. List -> Master -> Settings
  * List name & defaults
    * Turn off: Send a final welcome email
  * List fields and *|MERGE|* tags
    * Name           | text     | y | y | NAME
    * Newsletter     | number   | y | n | NEWSLETTER
    * Account Status | dropdown | y | n | ACCT
        Never, Active
  * Required email footer content <- fill in as appropriate


## FreshDesk

https://shipreq.freshdesk.com

1. Email Settings
    * Global Support Emails --> ShipReq <contact@shipreq.com>
    * BCC --> archive@shipreq.com
2. Email Notifications
    * Agent Notifications > New Ticket Created > Edit
      * Notification: On
      * Subject: [ShipReq Support] New ticket: (#{{ticket.id}}) {{ticket.subject}}
      * Notify Agents > Add
    * Requestor Notifications --> All off
3. Ticket Fields
    * Type --> [x] Required when closing, RFI|RFC|Incident|Problem|Lead|Other
4. Security
    * SSL --> On


## MailGun

Add `mg.shipreq.com` as a domain and follow the instructions
(which are just adding DNS records for DKIM).


# Google Analytics

Currently under account of japgolly@gmail.com.
There is a User Management tab so that I can give others access.
Long term, GA supports moving buckets ("Properties") to different accounts:
https://stackoverflow.com/questions/26386483/is-there-a-way-to-move-google-analytics-property-to-a-new-account

1. Goto https://analytics.google.com
2. On the left-side panel, open Admin.
3. Create account called ShipReq. It will be created with one Property.
4. Edit the Property and
  * rename it to "ShipReq: Production".
  * enable "Enable Users Metric in Reporting"
5. Create a new Property
  * name = "ShipReq: Test"
  * url = "http://local.shipreq.com"
  * enable "Enable Users Metric in Reporting"
6. For both properties, add custom dimensions and metrics as described below.

#### Custom dimensions

These can be set up on Google Analytics by going to the `Admin` section, clicking on `Custom Definitions` in the `PROPERTY` column will reveal a link to `Custom Dimensions`.

The index values are important as they align with the values in `../Code/frontend/shipreq/js/analytics.js`.

| Name             | Index | Scope |
| :--------------- | :---- | :---- |
| Tracking Version | 1     | Hit   |
| Client ID        | 2     | User  |
| Window ID        | 3     | Hit   |
| Hit ID           | 4     | Hit   |
| Hit Time         | 5     | Hit   |
| Hit Type         | 6     | Hit   |
| Hit Source       | 7     | Hit   |
| Visibility State | 8     | Hit   |

#### Custom metrics

These can be set up on Google Analytics by going to the `Admin` section, clicking on `Custom Definitions` in the `PROPERTY` column will reveal a link to `Custom Metrics`.

The index values are important as they align with the values in `../Code/frontend/shipreq/js/analytics.js`.

| Name              | Index | Scope | Formatting Type |
| :---------------- | :---- | :---- | :-------------- |
| Response End Time | 1     | Hit   | Integer         |
| DOM Load Time     | 2     | Hit   | Integer         |
| Window Load Time  | 3     | Hit   | Integer         |
| Page Visible      | 4     | Hit   | Integer         |
| Page Loads        | 5     | Hit   | Integer         |
