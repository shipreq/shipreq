-- Generated for Schema 5.3
-- Username: test__xabcdefghijklmnopx__1000
-- Password: 71^[:q-.At*'.^7Vh>(^rEQ>yxOJ(WK/p/Zo.H+KA(4PT07Kp.`FYEG^4YDO)rQl=N<g@WDNpPGeOr<Px26F6@GDA:wG3pwy50CRJk

-- http://localhost:8080/project/j8NA940XXv9
-- http://localhost:8080/project/j8NA940XXv9/read
-- http://localhost:8080/usecase/2PbB1awttl1
-- http://localhost:8080/usecase/2PbB10XLd8j


BEGIN TRANSACTION;

--ALTER TABLE usr DISABLE TRIGGER ALL;
INSERT INTO usr (id, username, email, password, password_salt, password_changed_at, confirmation_token, confirmation_sent_at, confirmed_at, reset_password_token, reset_password_sent_at, reset_password_req_count, login_count, last_login_at, last_login_ip) VALUES (-1000, 'test__xabcdefghijklmnopx__1000', 'japgolly+test__xabcdefghijklmnopx__1000@gmail.com', 'eyLhzz11LT2Z5eK648KTk1ar45U94t5u5wQp1ho7YiH5TDM8EkHshQL5ytw30mgnawe9HS1ZH3wJNVw/sOPdFg==', 'Fq4FQFLdd2LYfunPAGwLpQ==', '2013-11-25 10:49:19.043876+11', NULL, '2013-11-25 10:48:41.980779+11', '2013-11-25 10:49:19.043876+11', NULL, NULL, 0, 2, '2013-11-25 10:58:45.902061+11', '0:0:0:0:0:0:0:1');
UPDATE usr SET roles = 'test' WHERE id = -1000;
--ALTER TABLE usr ENABLE TRIGGER ALL;

--ALTER TABLE usr_login_log DISABLE TRIGGER ALL;
INSERT INTO usr_login_log ("time", usr_id, ip) VALUES ('2013-11-25 10:58:45.902061+11', -1000, '0:0:0:0:0:0:0:1');
--ALTER TABLE usr_login_log ENABLE TRIGGER ALL;

INSERT INTO project (id, usr_id, name, created_at) VALUES (-1000, -1000, 'Test Project', '2013-11-25 10:49:59.106105+11');

--ALTER TABLE usecase DISABLE TRIGGER ALL;
INSERT INTO usecase (id, latest_rev_id, number, project_id) VALUES (-1000, -1001, 1, -1000);
INSERT INTO usecase (id, latest_rev_id, number, project_id) VALUES (-1001, -1005, 2, -1000);
--ALTER TABLE usecase ENABLE TRIGGER ALL;

--ALTER TABLE usecase_rev DISABLE TRIGGER ALL;
INSERT INTO usecase_rev (ident_id, rev, id, created_at, title) VALUES (-1000, 1, -1000, '2013-11-25 10:50:37.320876+11', 'Place Order');
INSERT INTO usecase_rev (ident_id, rev, id, created_at, title) VALUES (-1000, 2, -1001, '2013-11-25 10:57:10.549479+11', 'Place Order');
INSERT INTO usecase_rev (ident_id, rev, id, created_at, title) VALUES (-1001, 1, -1002, '2013-11-25 10:59:27.499855+11', 'Feature Sample');
INSERT INTO usecase_rev (ident_id, rev, id, created_at, title) VALUES (-1001, 2, -1003, '2013-11-25 11:07:58.987632+11', 'Feature Sample');
INSERT INTO usecase_rev (ident_id, rev, id, created_at, title) VALUES (-1001, 3, -1004, '2013-11-25 11:08:21.992198+11', 'Feature Sample');
INSERT INTO usecase_rev (ident_id, rev, id, created_at, title) VALUES (-1001, 4, -1005, '2013-11-25 11:09:33.550165+11', 'Feature Sample');
--ALTER TABLE usecase_rev ENABLE TRIGGER ALL;

INSERT INTO text (id, uc_id, fk_id) VALUES (-1000, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1001, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1002, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1003, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1004, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1005, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1006, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1007, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1008, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1009, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1010, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1011, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1012, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1013, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1014, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1015, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1016, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1017, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1018, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1019, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1020, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1021, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1022, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1023, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1024, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1025, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1026, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1027, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1028, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1029, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1030, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1031, -1000, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1032, -1000, (select fk_id from v_field_key where "desc"='Text: Description'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1033, -1000, (select fk_id from v_field_key where "desc"='Text: Notes and Issues'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1034, -1000, (select fk_id from v_field_key where "desc"='Text: Post-Conditions'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1035, -1000, (select fk_id from v_field_key where "desc"='Text: Pre-Conditions'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1036, -1000, (select fk_id from v_field_key where "desc"='Text: Actors'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1037, -1001, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1038, -1001, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1039, -1001, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1040, -1001, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1041, -1001, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1042, -1001, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1043, -1001, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1044, -1001, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1045, -1001, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1046, -1001, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1047, -1001, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1048, -1001, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1049, -1001, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1050, -1001, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1051, -1001, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1052, -1001, (select fk_id from v_field_key where "desc"='NormalAndAlternateCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1053, -1001, (select fk_id from v_field_key where "desc"='ExceptionCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1054, -1001, (select fk_id from v_field_key where "desc"='ExceptionCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1055, -1001, (select fk_id from v_field_key where "desc"='ExceptionCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1056, -1001, (select fk_id from v_field_key where "desc"='ExceptionCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1057, -1001, (select fk_id from v_field_key where "desc"='ExceptionCourses'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1058, -1001, (select fk_id from v_field_key where "desc"='Text: Description'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1059, -1001, (select fk_id from v_field_key where "desc"='Text: Post-Conditions'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1060, -1001, (select fk_id from v_field_key where "desc"='Text: Pre-Conditions'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1061, -1001, (select fk_id from v_field_key where "desc"='Text: Actors'));
INSERT INTO text (id, uc_id, fk_id) VALUES (-1062, -1001, (select fk_id from v_field_key where "desc"='Text: Use Case Relationships'));


INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1032, 1, -1000, '2013-11-25 10:57:10.549479+11', 'http://tynerblain.com/blog/2007/04/09/sample-use-case-example/');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1000, 1, -1001, '2013-11-25 10:57:10.549479+11', 'Place Order');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1001, 1, -1002, '2013-11-25 10:57:10.549479+11', 'The user will indicate that she wants to order the items that have already been selected.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1002, 1, -1003, '2013-11-25 10:57:10.549479+11', 'The system will present the billing and shipping information that the user previously stored. ➡ [D.-1018] [D.-1023] [D.-1028]');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1003, 1, -1004, '2013-11-25 10:57:10.549479+11', 'The user will confirm that the existing billing and shipping information should be used for this order. ⬅ [D.-1022] [D.-1027]');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1004, 1, -1005, '2013-11-25 10:57:10.549479+11', 'The system will present the amount that the order will cost, including applicable taxes and shipping charges.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1005, 1, -1006, '2013-11-25 10:57:10.549479+11', 'The user will confirm that the order information is accurate.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1006, 1, -1007, '2013-11-25 10:57:10.549479+11', 'The system will provide the user with a tracking ID for the order.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1007, 1, -1008, '2013-11-25 10:57:10.549479+11', 'The system will submit the order to the fulfillment system for evaluation.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1008, 1, -1009, '2013-11-25 10:57:10.549479+11', 'The fulfillment system will provide the system with an estimated delivery date.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1009, 1, -1010, '2013-11-25 10:57:10.549479+11', 'The system will present the estimated delivery date to the user.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1010, 1, -1011, '2013-11-25 10:57:10.549479+11', 'The user will indicate that the order should be placed.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1011, 1, -1012, '2013-11-25 10:57:10.549479+11', 'The system will request that the billing system should charge the user for the order.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1012, 1, -1013, '2013-11-25 10:57:10.549479+11', 'The billing system will confirm that the charge has been placed for the order.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1013, 1, -1014, '2013-11-25 10:57:10.549479+11', 'The system will submit the order to the fulfillment system for processing.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1014, 1, -1015, '2013-11-25 10:57:10.549479+11', 'The fulfillment system will confirm that the order is being processed.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1015, 1, -1016, '2013-11-25 10:57:10.549479+11', 'The system will indicate to the user that the user has been charged for the order.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1016, 1, -1017, '2013-11-25 10:57:10.549479+11', 'The system will indicate to the user that the order has been placed.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1017, 1, -1018, '2013-11-25 10:57:10.549479+11', 'The user will exit the system.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1018, 1, -1019, '2013-11-25 10:57:10.549479+11', 'The user desires to use shipping and billing information that differs from the information stored in her account. ⬅ [D.-1002]');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1019, 1, -1020, '2013-11-25 10:57:10.549479+11', 'The user will indicate that this order should use alternate billing or shipping information.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1020, 1, -1021, '2013-11-25 10:57:10.549479+11', 'The user will enter billing and shipping information for this order.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1021, 1, -1022, '2013-11-25 10:57:10.549479+11', 'The system will validate the billing and shipping information. ⬅ [D.-1031]');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1022, 1, -1023, '2013-11-25 10:57:10.549479+11', 'The use case continues. ➡ [D.-1003]');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1023, 1, -1024, '2013-11-25 10:57:10.549479+11', 'The user will discover an error in the billing or shipping information associated with their account, and will change it. ⬅ [D.-1002]');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1024, 1, -1025, '2013-11-25 10:57:10.549479+11', 'The user will indicate that the billing and shipping information is incorrect.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1025, 1, -1026, '2013-11-25 10:57:10.549479+11', 'The user will edit the billing and shipping information associated with their account.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1026, 1, -1027, '2013-11-25 10:57:10.549479+11', 'The system will validate the billing and shipping information.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1027, 1, -1028, '2013-11-25 10:57:10.549479+11', 'The use case continues. ➡ [D.-1003]');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1028, 1, -1029, '2013-11-25 10:57:10.549479+11', 'The user will discover an error in the billing or shipping information that is uniquely being used for this order, and will change it. ⬅ [D.-1002]');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1029, 1, -1030, '2013-11-25 10:57:10.549479+11', 'The user will indicate that the billing and shipping information is incorrect.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1030, 1, -1031, '2013-11-25 10:57:10.549479+11', 'The user will edit the billing and shipping information for this order.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1031, 1, -1032, '2013-11-25 10:57:10.549479+11', 'The use case returns to step 3A1 step 3. ➡ [D.-1021]');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1033, 1, -1033, '2013-11-25 10:57:10.549479+11', 'Trigger: The user indicates that she wants to purchase items that she has selected.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1034, 1, -1034, '2013-11-25 10:57:10.549479+11', '* The order will be placed in the system.
* The user will have a tracking ID for the order.
* The user will know the estimated delivery date for the order.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1035, 1, -1035, '2013-11-25 10:57:10.549479+11', 'User has selected the items to be purchased.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1036, 1, -1036, '2013-11-25 10:57:10.549479+11', '* Registered Shopper (Has an existing account, possibly with billing and shipping information)
* Non-registered Shopper (Does not have an existing account)
* Fulfillment System (processes orders for delivery to customers)
* Billing System (bills customers for orders that have been placed)');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1058, 1, -1037, '2013-11-25 11:07:58.987632+11', 'Look at these steps: [D.-1050] [D.-1055]');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1037, 1, -1038, '2013-11-25 11:07:58.987632+11', 'Feature Sample');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1038, 1, -1039, '2013-11-25 11:07:58.987632+11', 'Blah');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1039, 1, -1040, '2013-11-25 11:07:58.987632+11', 'Blah again ⬅ [D.-1055] ➡ [D.-1047]');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1040, 1, -1041, '2013-11-25 11:07:58.987632+11', 'Let''s have children');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1041, 1, -1042, '2013-11-25 11:07:58.987632+11', 'Little Johhny');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1042, 1, -1043, '2013-11-25 11:07:58.987632+11', 'Little Johno');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1043, 1, -1044, '2013-11-25 11:07:58.987632+11', 'Little J');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1044, 1, -1045, '2013-11-25 11:07:58.987632+11', 'Part 1');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1045, 1, -1046, '2013-11-25 11:07:58.987632+11', 'Part 2');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1046, 1, -1047, '2013-11-25 11:07:58.987632+11', 'Aaaaaaaaand we''re done. ⬅ [D.-1049] [D.-1052]');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1047, 1, -1048, '2013-11-25 11:07:58.987632+11', 'Skip children ⬅ [D.-1039]');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1048, 1, -1049, '2013-11-25 11:07:58.987632+11', 'Broadcast!!');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1049, 1, -1050, '2013-11-25 11:07:58.987632+11', 'Resume ➡ [D.-1046]');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1050, 1, -1051, '2013-11-25 11:07:58.987632+11', 'BYPASS EVERYTHING!');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1051, 1, -1052, '2013-11-25 11:07:58.987632+11', 'Skipping the queue...');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1052, 1, -1053, '2013-11-25 11:07:58.987632+11', 'End of the line: ➡ [D.-1046]');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1059, 1, -1054, '2013-11-25 11:07:58.987632+11', 'Do you like math? I''ve heard that: {|math: e = mc^2 |}');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1060, 1, -1055, '2013-11-25 11:07:58.987632+11', 'I used to referenced [DELETED] but then it was removed.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1061, 1, -1056, '2013-11-25 11:07:58.987632+11', '* I''m list item #1
* I''m list item #2

Blank lines!!! And math: {|math: 1 \over m + n |} just like {|math: O(n log n) |}

* More listy-ness');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1053, 1, -1057, '2013-11-25 11:07:58.987632+11', 'Broken');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1054, 1, -1058, '2013-11-25 11:07:58.987632+11', 'Blah ➡ [D.-1056]');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1055, 1, -1059, '2013-11-25 11:07:58.987632+11', 'Fix yourself! ➡ [D.-1039]');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1056, 1, -1060, '2013-11-25 11:07:58.987632+11', 'Some other error ⬅ [D.-1054]');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1057, 1, -1061, '2013-11-25 11:07:58.987632+11', 'Give up.');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1061, 2, -1062, '2013-11-25 11:08:21.992198+11', '* I''m list item #1
* I''m list item #2

Blank lines!!! And math: {|math: 1 \over m + n |} just like {|math: P(E) = {n \choose k} p^k (1-p)^{n-k} |}

* More listy-ness');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1058, 2, -1063, '2013-11-25 11:09:33.550165+11', 'Look at these steps: [D.-1050] and [D.-1055].
So much better than [UC-1]!!');
INSERT INTO text_rev (ident_id, rev, id, created_at, text) VALUES (-1062, 1, -1064, '2013-11-25 11:09:33.550165+11', '* [UC-1] blah blah.
* [UC-100?] too.');

--ALTER TABLE uc_field DISABLE TRIGGER ALL;
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, NULL, NULL, -1, -1000);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.0', NULL, 0, -1001);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.0.1', -1001, 0, -1002);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.0.2', -1001, 1, -1003);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.0.3', -1001, 2, -1004);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.0.4', -1001, 3, -1005);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.0.5', -1001, 4, -1006);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.0.6', -1001, 5, -1007);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.0.7', -1001, 6, -1008);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.0.8', -1001, 7, -1009);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.0.9', -1001, 8, -1010);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.0.10', -1001, 9, -1011);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.0.11', -1001, 10, -1012);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.0.12', -1001, 11, -1013);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.0.13', -1001, 12, -1014);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.0.14', -1001, 13, -1015);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.0.15', -1001, 14, -1016);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.0.16', -1001, 15, -1017);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.0.17', -1001, 16, -1018);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.1', NULL, 1, -1019);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.1.1', -1019, 0, -1020);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.1.2', -1019, 1, -1021);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.1.3', -1019, 2, -1022);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.1.4', -1019, 3, -1023);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.2', NULL, 2, -1024);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.2.1', -1024, 0, -1025);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.2.2', -1024, 1, -1026);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.2.3', -1024, 2, -1027);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.2.4', -1024, 3, -1028);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.3', NULL, 3, -1029);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.3.1', -1029, 0, -1030);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.3.2', -1029, 1, -1031);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, '1.3.3', -1029, 2, -1032);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, NULL, NULL, -1, -1033);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, NULL, NULL, -1, -1034);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, NULL, NULL, -1, -1035);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1001, NULL, NULL, -1, -1036);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, NULL, NULL, -1, -1037);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, '2.0', NULL, 0, -1038);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, '2.0.1', -1038, 0, -1039);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, '2.0.2', -1038, 1, -1040);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, '2.0.3', -1038, 2, -1041);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, '2.0.3.a', -1041, 0, -1042);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, '2.0.3.b', -1041, 1, -1043);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, '2.0.3.c', -1041, 2, -1044);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, '2.0.3.c.i', -1044, 0, -1045);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, '2.0.3.c.ii', -1044, 1, -1046);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, '2.0.4', -1038, 3, -1047);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, '2.1', NULL, 1, -1048);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, '2.1.1', -1048, 0, -1049);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, '2.1.2', -1048, 1, -1050);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, '2.2', NULL, 2, -1051);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, '2.2.1', -1051, 0, -1052);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, '2.2.2', -1051, 1, -1053);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, NULL, NULL, -1, -1054);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, NULL, NULL, -1, -1055);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, NULL, NULL, -1, -1056);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, '2.E.1', NULL, 0, -1057);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, '2.E.1.1', -1057, 0, -1058);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, '2.E.1.2', -1057, 1, -1059);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, '2.E.2', NULL, 1, -1060);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1003, '2.E.2.1', -1060, 0, -1061);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, NULL, NULL, -1, -1037);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, '2.0', NULL, 0, -1038);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, '2.0.1', -1038, 0, -1039);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, '2.0.2', -1038, 1, -1040);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, '2.0.3', -1038, 2, -1041);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, '2.0.3.a', -1041, 0, -1042);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, '2.0.3.b', -1041, 1, -1043);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, '2.0.3.c', -1041, 2, -1044);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, '2.0.3.c.i', -1044, 0, -1045);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, '2.0.3.c.ii', -1044, 1, -1046);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, '2.0.4', -1038, 3, -1047);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, '2.1', NULL, 1, -1048);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, '2.1.1', -1048, 0, -1049);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, '2.1.2', -1048, 1, -1050);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, '2.2', NULL, 2, -1051);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, '2.2.1', -1051, 0, -1052);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, '2.2.2', -1051, 1, -1053);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, NULL, NULL, -1, -1054);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, NULL, NULL, -1, -1055);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, NULL, NULL, -1, -1062);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, '2.E.1', NULL, 0, -1057);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, '2.E.1.1', -1057, 0, -1058);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, '2.E.1.2', -1057, 1, -1059);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, '2.E.2', NULL, 1, -1060);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1004, '2.E.2.1', -1060, 0, -1061);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, NULL, NULL, -1, -1063);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, '2.0', NULL, 0, -1038);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, '2.0.1', -1038, 0, -1039);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, '2.0.2', -1038, 1, -1040);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, '2.0.3', -1038, 2, -1041);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, '2.0.3.a', -1041, 0, -1042);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, '2.0.3.b', -1041, 1, -1043);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, '2.0.3.c', -1041, 2, -1044);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, '2.0.3.c.i', -1044, 0, -1045);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, '2.0.3.c.ii', -1044, 1, -1046);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, '2.0.4', -1038, 3, -1047);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, '2.1', NULL, 1, -1048);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, '2.1.1', -1048, 0, -1049);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, '2.1.2', -1048, 1, -1050);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, '2.2', NULL, 2, -1051);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, '2.2.1', -1051, 0, -1052);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, '2.2.2', -1051, 1, -1053);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, NULL, NULL, -1, -1054);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, NULL, NULL, -1, -1055);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, NULL, NULL, -1, -1064);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, NULL, NULL, -1, -1062);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, '2.E.1', NULL, 0, -1057);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, '2.E.1.1', -1057, 0, -1058);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, '2.E.1.2', -1057, 1, -1059);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, '2.E.2', NULL, 1, -1060);
INSERT INTO uc_field (uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES (-1005, '2.E.2.1', -1060, 0, -1061);
--ALTER TABLE uc_field ENABLE TRIGGER ALL;

COMMIT;

