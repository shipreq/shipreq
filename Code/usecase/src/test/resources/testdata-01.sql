--
-- Data for Name: data_type; Type: TABLE DATA; Schema: public; Owner: usecase
--

INSERT INTO data_type (id, name) VALUES (100, 'UseCase');
INSERT INTO data_type (id, name) VALUES (101, 'FieldList');
INSERT INTO data_type (id, name) VALUES (102, 'FieldKey');
INSERT INTO data_type (id, name) VALUES (103, 'FieldValue');
INSERT INTO data_type (id, name) VALUES (104, 'Step');


--
-- Data for Name: data; Type: TABLE DATA; Schema: public; Owner: usecase
--

INSERT INTO data (id, type_id) VALUES (1, 101);
INSERT INTO data (id, type_id) VALUES (100, 102);
INSERT INTO data (id, type_id) VALUES (101, 102);
INSERT INTO data (id, type_id) VALUES (102, 102);
INSERT INTO data (id, type_id) VALUES (103, 102);
INSERT INTO data (id, type_id) VALUES (104, 102);
INSERT INTO data (id, type_id) VALUES (105, 102);
INSERT INTO data (id, type_id) VALUES (106, 102);
INSERT INTO data (id, type_id) VALUES (107, 102);
INSERT INTO data (id, type_id) VALUES (108, 102);
INSERT INTO data (id, type_id) VALUES (109, 102);
INSERT INTO data (id, type_id) VALUES (110, 102);
INSERT INTO data (id, type_id) VALUES (111, 100);
INSERT INTO data (id, type_id) VALUES (112, 103);
INSERT INTO data (id, type_id) VALUES (113, 103);
INSERT INTO data (id, type_id) VALUES (114, 103);
INSERT INTO data (id, type_id) VALUES (115, 104);
INSERT INTO data (id, type_id) VALUES (116, 104);
INSERT INTO data (id, type_id) VALUES (117, 104);
INSERT INTO data (id, type_id) VALUES (118, 104);
INSERT INTO data (id, type_id) VALUES (119, 104);
INSERT INTO data (id, type_id) VALUES (120, 104);
INSERT INTO data (id, type_id) VALUES (121, 104);
INSERT INTO data (id, type_id) VALUES (122, 104);
INSERT INTO data (id, type_id) VALUES (123, 104);
INSERT INTO data (id, type_id) VALUES (124, 104);
INSERT INTO data (id, type_id) VALUES (125, 104);
INSERT INTO data (id, type_id) VALUES (126, 104);
INSERT INTO data (id, type_id) VALUES (127, 104);
INSERT INTO data (id, type_id) VALUES (128, 104);
INSERT INTO data (id, type_id) VALUES (129, 104);
INSERT INTO data (id, type_id) VALUES (130, 103);
INSERT INTO data (id, type_id) VALUES (131, 104);
INSERT INTO data (id, type_id) VALUES (132, 104);
INSERT INTO data (id, type_id) VALUES (133, 104);
INSERT INTO data (id, type_id) VALUES (134, 104);
INSERT INTO data (id, type_id) VALUES (135, 103);
INSERT INTO data (id, type_id) VALUES (136, 103);
INSERT INTO data (id, type_id) VALUES (137, 103);
INSERT INTO data (id, type_id) VALUES (138, 100);
INSERT INTO data (id, type_id) VALUES (139, 103);
INSERT INTO data (id, type_id) VALUES (140, 103);


--
-- Name: data_seq; Type: SEQUENCE SET; Schema: public; Owner: usecase
--

SELECT pg_catalog.setval('data_seq', 140, true);


--
-- Data for Name: field_key_type; Type: TABLE DATA; Schema: public; Owner: usecase
--

INSERT INTO field_key_type (id, name) VALUES (300, 'Text');
INSERT INTO field_key_type (id, name) VALUES (301, 'NormalAndAlternateCourses');
INSERT INTO field_key_type (id, name) VALUES (302, 'ExceptionCourses');


--
-- Data for Name: value; Type: TABLE DATA; Schema: public; Owner: usecase
--

INSERT INTO value (id, data_id, rev, updated_at) VALUES (1000, 1, 1, '2013-07-09 12:46:12.147845+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1001, 100, 1, '2013-07-09 12:46:12.147845+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1002, 101, 1, '2013-07-09 12:46:12.147845+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1003, 102, 1, '2013-07-09 12:46:12.147845+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1004, 103, 1, '2013-07-09 12:46:12.147845+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1005, 104, 1, '2013-07-09 12:46:12.147845+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1006, 105, 1, '2013-07-09 12:46:12.147845+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1007, 106, 1, '2013-07-09 12:46:12.147845+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1008, 107, 1, '2013-07-09 12:46:12.147845+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1009, 108, 1, '2013-07-09 12:46:12.147845+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1010, 109, 1, '2013-07-09 12:46:12.147845+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1011, 110, 1, '2013-07-09 12:46:12.147845+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1012, 111, 1, '2013-07-10 10:29:49.381655+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1013, 112, 1, '2013-07-10 10:47:17.857292+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1014, 113, 1, '2013-07-10 10:47:17.857292+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1015, 114, 1, '2013-07-10 10:47:17.857292+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1016, 115, 1, '2013-07-10 10:47:17.857292+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1017, 116, 1, '2013-07-10 10:47:17.857292+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1018, 117, 1, '2013-07-10 10:47:17.857292+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1019, 118, 1, '2013-07-10 10:47:17.857292+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1020, 119, 1, '2013-07-10 10:47:17.857292+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1021, 120, 1, '2013-07-10 10:47:17.857292+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1022, 121, 1, '2013-07-10 10:47:17.857292+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1023, 122, 1, '2013-07-10 10:47:17.857292+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1024, 123, 1, '2013-07-10 10:47:17.857292+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1025, 124, 1, '2013-07-10 10:47:17.857292+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1026, 125, 1, '2013-07-10 10:47:17.857292+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1027, 126, 1, '2013-07-10 10:47:17.857292+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1028, 127, 1, '2013-07-10 10:47:17.857292+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1029, 128, 1, '2013-07-10 10:47:17.857292+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1030, 129, 1, '2013-07-10 10:47:17.857292+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1031, 130, 1, '2013-07-10 10:47:17.857292+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1032, 111, 2, '2013-07-10 10:47:17.857292+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1033, 119, 2, '2013-07-10 10:59:03.302153+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1034, 120, 2, '2013-07-10 10:59:03.302153+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1035, 126, 2, '2013-07-10 10:59:03.302153+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1036, 130, 2, '2013-07-10 10:59:03.302153+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1037, 131, 1, '2013-07-10 10:59:03.302153+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1038, 132, 1, '2013-07-10 10:59:03.302153+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1039, 133, 1, '2013-07-10 10:59:03.302153+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1040, 134, 1, '2013-07-10 10:59:03.302153+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1041, 135, 1, '2013-07-10 10:59:03.302153+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1042, 136, 1, '2013-07-10 10:59:03.302153+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1043, 111, 3, '2013-07-10 10:59:03.302153+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1044, 137, 1, '2013-07-10 11:01:40.279928+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1046, 138, 1, '2013-07-10 11:12:55.789908+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1047, 126, 3, '2013-07-10 11:17:41.993265+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1048, 139, 1, '2013-07-10 11:17:41.993265+10');
INSERT INTO value (id, data_id, rev, updated_at) VALUES (1050, 140, 1, '2013-07-10 11:23:31.010936+10');


--
-- Data for Name: field_key; Type: TABLE DATA; Schema: public; Owner: usecase
--

INSERT INTO field_key (id, type_id, data) VALUES (1001, 300, 'Actors');
INSERT INTO field_key (id, type_id, data) VALUES (1002, 300, 'Pre-Conditions');
INSERT INTO field_key (id, type_id, data) VALUES (1003, 300, 'Post-Conditions');
INSERT INTO field_key (id, type_id, data) VALUES (1004, 301, NULL);
INSERT INTO field_key (id, type_id, data) VALUES (1005, 302, NULL);
INSERT INTO field_key (id, type_id, data) VALUES (1006, 300, 'Use Case Relationships');
INSERT INTO field_key (id, type_id, data) VALUES (1007, 300, 'Constraints and Business Rules');
INSERT INTO field_key (id, type_id, data) VALUES (1008, 300, 'Frequency of Use');
INSERT INTO field_key (id, type_id, data) VALUES (1009, 300, 'Special Requirements');
INSERT INTO field_key (id, type_id, data) VALUES (1010, 300, 'Assumptions');
INSERT INTO field_key (id, type_id, data) VALUES (1011, 300, 'Notes and Issues');


--
-- Data for Name: field_value; Type: TABLE DATA; Schema: public; Owner: usecase
--

INSERT INTO field_value (id, field_key_id, text) VALUES (1013, 1001, 'BA');
INSERT INTO field_value (id, field_key_id, text) VALUES (1014, 1002, 'BA is signed in.');
INSERT INTO field_value (id, field_key_id, text) VALUES (1015, 1003, 'A new project exists.');
INSERT INTO field_value (id, field_key_id, text) VALUES (1031, 1004, NULL);
INSERT INTO field_value (id, field_key_id, text) VALUES (1036, 1004, NULL);
INSERT INTO field_value (id, field_key_id, text) VALUES (1041, 1005, NULL);
INSERT INTO field_value (id, field_key_id, text) VALUES (1042, 1008, 'P(50%): 0 times (provided a default project is created on signup, else once).
P(90%): ≤ 10 times or less.');
INSERT INTO field_value (id, field_key_id, text) VALUES (1044, 1008, 'P(50%): Never (provided a default project is created on signup, else once).
P(90%): 0 to 10 times.');
INSERT INTO field_value (id, field_key_id, text) VALUES (1048, 1004, NULL);
INSERT INTO field_value (id, field_key_id, text) VALUES (1050, 1008, 'P(50%): Never (provided a default project is created on signup, else once).
P(90%): 0 to 10 times.');


--
-- Data for Name: relation_type; Type: TABLE DATA; Schema: public; Owner: usecase
--

INSERT INTO relation_type (id, name) VALUES (200, 'Has');
INSERT INTO relation_type (id, name) VALUES (201, 'References');


--
-- Data for Name: relation; Type: TABLE DATA; Schema: public; Owner: usecase
--

INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1000, 200, 0, 1001);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1000, 200, 1, 1002);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1000, 200, 2, 1003);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1000, 200, 3, 1004);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1000, 200, 4, 1005);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1000, 200, 5, 1006);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1000, 200, 6, 1007);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1000, 200, 7, 1008);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1000, 200, 8, 1009);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1000, 200, 9, 1010);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1000, 200, 10, 1011);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1020, 200, 0, 1018);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1020, 200, 1, 1019);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1027, 200, 0, 1016);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1027, 200, 1, 1017);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1027, 200, 2, 1020);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1027, 200, 3, 1021);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1027, 200, 4, 1024);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1027, 200, 5, 1025);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1027, 200, 6, 1026);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1024, 200, 0, 1022);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1024, 200, 1, 1023);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1030, 200, 0, 1028);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1030, 200, 1, 1029);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1031, 200, 0, 1027);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1031, 200, 1, 1030);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1032, 200, -1, 1013);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1032, 200, -1, 1014);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1032, 200, -1, 1015);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1032, 200, -1, 1031);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1033, 200, 0, 1018);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1035, 200, 0, 1016);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1035, 200, 1, 1017);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1035, 200, 2, 1033);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1035, 200, 3, 1034);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1035, 200, 4, 1025);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1035, 200, 5, 1026);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1036, 200, 0, 1035);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1036, 200, 1, 1030);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1038, 200, 0, 1037);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1040, 200, 0, 1039);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1041, 200, 0, 1038);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1041, 200, 1, 1040);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1043, 200, -1, 1041);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1043, 200, -1, 1042);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1043, 200, -1, 1015);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1043, 200, -1, 1036);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1043, 200, -1, 1013);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1043, 200, -1, 1014);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1047, 200, 0, 1016);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1047, 200, 1, 1017);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1047, 200, 2, 1033);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1047, 200, 3, 1034);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1047, 200, 4, 1025);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1047, 200, 5, 1026);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1048, 200, 0, 1047);
INSERT INTO relation (from_id, type_id, index, to_id) VALUES (1048, 200, 1, 1030);


--
-- Data for Name: step; Type: TABLE DATA; Schema: public; Owner: usecase
--

INSERT INTO step (id, text) VALUES (1021, 'BA fills out form and submits. ⬅ [D.123] ➡ [D.129]');
INSERT INTO step (id, text) VALUES (1028, 'BA indicates cancel.');
INSERT INTO step (id, text) VALUES (1020, 'System presents BA with project detail form consisting of:');
INSERT INTO step (id, text) VALUES (1018, 'Title');
INSERT INTO step (id, text) VALUES (1025, 'System creates new project.');
INSERT INTO step (id, text) VALUES (1022, 'Validates: User doesn''t have any other projects with the same name.');
INSERT INTO step (id, text) VALUES (1027, 'Create a Project');
INSERT INTO step (id, text) VALUES (1024, 'System validates form details. If invalid ➡ [D.120]');
INSERT INTO step (id, text) VALUES (1017, 'BA indicates new project.');
INSERT INTO step (id, text) VALUES (1019, 'Description');
INSERT INTO step (id, text) VALUES (1030, 'BA changes mind. ⬅ [D.120]');
INSERT INTO step (id, text) VALUES (1016, 'System displays BA''s projects. ⬅ [D.128]');
INSERT INTO step (id, text) VALUES (1026, 'System directs BA to the view-project page.');
INSERT INTO step (id, text) VALUES (1023, 'Validates: Title isn''t empty.');
INSERT INTO step (id, text) VALUES (1029, 'System removes form. ➡ [D.115]');
INSERT INTO step (id, text) VALUES (1034, 'BA fills out form and submits. ⬅ [D.131] [D.133] ➡ [D.129] [D.132] [D.134]');
INSERT INTO step (id, text) VALUES (1033, 'System presents BA with project detail form consisting of:');
INSERT INTO step (id, text) VALUES (1035, 'Create a Project');
INSERT INTO step (id, text) VALUES (1037, 'System indicates thus. ➡ [D.120]');
INSERT INTO step (id, text) VALUES (1038, 'Project name is empty. ⬅ [D.120]');
INSERT INTO step (id, text) VALUES (1040, 'BA already has a project with the same name. ⬅ [D.120]');
INSERT INTO step (id, text) VALUES (1039, 'System indicates thus. ➡ [D.120]');
INSERT INTO step (id, text) VALUES (1047, 'Create a Project!');


--
-- Data for Name: usecase; Type: TABLE DATA; Schema: public; Owner: usecase
--

INSERT INTO usecase (id, title, number, field_list_id) VALUES (1012, 'Create a Project', 1, 1000);
INSERT INTO usecase (id, title, number, field_list_id) VALUES (1032, 'Create a Project', 1, 1000);
INSERT INTO usecase (id, title, number, field_list_id) VALUES (1043, 'Create a Project', 1, 1000);
INSERT INTO usecase (id, title, number, field_list_id) VALUES (1046, 'Untitled', 2, 1000);


--
-- Data for Name: usr; Type: TABLE DATA; Schema: public; Owner: usecase
--

INSERT INTO usr (id, username, email, password, password_salt, password_changed_at, confirmation_token, confirmation_sent_at, confirmed_at, reset_password_token, reset_password_sent_at, reset_password_req_count, login_count, last_login_at, last_login_ip) VALUES (1, 'golly', 'japgolly@gmail.com', 'YWe4vUlWzGJV3pDkJOn0CDid9jYSHQbU8yvqHuAOfFwehyUFQ4yBZy5z19AC/pXE6/y2/DHJpqMQJVq4tBuQBA==', 'eQUHN2pjvU625AjgJJnGNw==', '2013-07-10 10:29:09.442563+10', NULL, '2013-07-10 10:27:08.368719+10', '2013-07-10 10:29:09.442563+10', NULL, NULL, 0, 1, '2013-07-10 10:29:09.442563+10', '0:0:0:0:0:0:0:1');


--
-- Name: usr_seq; Type: SEQUENCE SET; Schema: public; Owner: usecase
--

SELECT pg_catalog.setval('usr_seq', 1, true);


--
-- Name: value_seq; Type: SEQUENCE SET; Schema: public; Owner: usecase
--

SELECT pg_catalog.setval('value_seq', 1051, true);


--
-- PostgreSQL database dump complete
--

