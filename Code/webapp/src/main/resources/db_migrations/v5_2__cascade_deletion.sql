-- Delete usecase cascades to usecase_rev, text, text_rev, uc_field

ALTER TABLE usecase_rev
  DROP CONSTRAINT usecase_rev_ident_id_fkey
  ,ADD CONSTRAINT usecase_rev_ident_id_fkey FOREIGN KEY (ident_id) REFERENCES usecase (id) ON DELETE CASCADE;

ALTER TABLE text
  DROP CONSTRAINT text_uc_id_fkey
  ,ADD CONSTRAINT text_uc_id_fkey FOREIGN KEY (uc_id) REFERENCES usecase (id) ON DELETE CASCADE;

ALTER TABLE text_rev
  DROP CONSTRAINT text_rev_ident_id_fkey
  ,ADD CONSTRAINT text_rev_ident_id_fkey FOREIGN KEY (ident_id) REFERENCES text (id) ON DELETE CASCADE;

ALTER TABLE uc_field
  DROP CONSTRAINT uc_field_uc_rev_id_fkey
  ,ADD CONSTRAINT uc_field_uc_rev_id_fkey FOREIGN KEY (uc_rev_id) REFERENCES usecase_rev (id) ON DELETE CASCADE;

-- Delete project cascades to usecase, share

ALTER TABLE usecase
  DROP CONSTRAINT usecase_project_id_fkey
  ,ADD CONSTRAINT usecase_project_id_fkey FOREIGN KEY (project_id) REFERENCES project (id) ON DELETE CASCADE;

ALTER TABLE share
  DROP CONSTRAINT share_project_id_fkey
  ,ADD CONSTRAINT share_project_id_fkey FOREIGN KEY (project_id) REFERENCES project (id) ON DELETE CASCADE;

-- Delete usr cascades to project

ALTER TABLE project
  DROP CONSTRAINT project_usr_id_fkey
  ,ADD CONSTRAINT project_usr_id_fkey FOREIGN KEY (usr_id) REFERENCES usr (id) ON DELETE CASCADE;
