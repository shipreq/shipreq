-- 1. Add project ids to usecases.
-- 2. Create project for existing user (me) and use cases
-- 3. Make UC numbers unique per project.

/*
INSERT INTO usr (id, username, email, password, password_salt, password_changed_at, confirmation_token, confirmation_sent_at, confirmed_at, reset_password_token, reset_password_sent_at, reset_password_req_count, login_count, last_login_at, last_login_ip) VALUES (1, 'golly', 'japgolly@gmail.com', 'YWe4vUlWzGJV3pDkJOn0CDid9jYSHQbU8yvqHuAOfFwehyUFQ4yBZy5z19AC/pXE6/y2/DHJpqMQJVq4tBuQBA==', 'eQUHN2pjvU625AjgJJnGNw==', '2013-07-10 10:29:09.442563+10', NULL, '2013-07-10 10:27:08.368719+10', '2013-07-10 10:29:09.442563+10', NULL, NULL, 0, 4, '2013-09-09 12:08:33.458792+10', '0:0:0:0:0:0:0:1');
INSERT INTO usecase (id, latest_rev_id) VALUES (1, null);
select * from usr
select * from usecase
select * from project
*/

INSERT INTO project(usr_id,name) SELECT id, 'Untitled Project' FROM usr WHERE username IS NOT NULL;

ALTER TABLE usecase ADD COLUMN project_id BIGINT;

UPDATE usecase SET project_id = (SELECT id FROM project);

ALTER TABLE usecase
  ALTER COLUMN project_id SET NOT NULL,
  ADD CONSTRAINT usecase_project_id_fkey FOREIGN KEY (project_id) REFERENCES project(id),
  ADD CONSTRAINT usecase_unique_number_per_project UNIQUE(project_id, number);
