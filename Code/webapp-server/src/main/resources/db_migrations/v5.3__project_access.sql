CREATE TYPE project_perm AS ENUM ('admin', 'collaborator');

CREATE TABLE project_access (
  project_id BIGINT NOT NULL REFERENCES project,
  usr_id     BIGINT NOT NULL REFERENCES usr,
  perm       project_perm NOT NULL,
  UNIQUE(project_id, usr_id) -- only one perm is permitted
);

CREATE INDEX project_access_idx_by_usr ON project_access(usr_id, project_id);

------------------------------------------------------------------------------------------------------------------------

CREATE OR REPLACE VIEW projects_by_owner_type AS
  SELECT user_type_by_username(username) owner_type,
         hll_add_agg(hll_hash_bigint(project_id)) projects,
         count(1) count
    FROM project_access a, usr u
   WHERE a.usr_id = u.id
     AND username IS NOT NULL
   GROUP BY user_type_by_username(username);

------------------------------------------------------------------------------------------------------------------------

INSERT INTO project_access
SELECT p.id AS project_id,
       p.usr_id,
       'admin'::project_perm AS perm
  FROM project p;

ALTER TABLE project RENAME COLUMN usr_id TO creator_id;
