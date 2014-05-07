ShipReq Production Deployment Instructions
==========================================

1. [Setup AWS](aws/README.md).
2. [Setup OS & environment](env/README.md).
3. [Deploy ShipReq Webapp](webapp/README.md).
4. [Deploy Taskman](taskman/README.md).


Third-Party Setup
=================

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

1. Email Settings
    * Global Support Emails --> ShipReq <contact@shipreq.com>
    * BCC --> shipreq@gmail.com
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

