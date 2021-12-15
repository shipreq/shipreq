ALTER TABLE global_event ADD COLUMN project_id BIGINT NULL REFERENCES project;

CREATE INDEX global_event_idx_by_project ON global_event(project_id);
CREATE INDEX global_event_idx_by_usr     ON global_event(usr_id);
