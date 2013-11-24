/*
DROP TRIGGER  IF EXISTS enforce_uc_field_integrity ON uc_field;
DROP TRIGGER  IF EXISTS latest_usecase_rev_trigger_1 ON usecase_rev;
DROP TRIGGER  IF EXISTS latest_usecase_rev_trigger_2 ON usecase_rev;
DROP TRIGGER  IF EXISTS enforce_max_text_per_fk_per_uc ON text;
DROP FUNCTION IF EXISTS check_uc_field_integrity();
DROP FUNCTION IF EXISTS check_max_text_per_fk_per_uc();
DROP FUNCTION IF EXISTS update_latest_usecase_rev_1();
DROP FUNCTION IF EXISTS update_latest_usecase_rev_2();
*/

------------------------------------------------------------------------------------------------------------------------
-- Trigger: Enforce a FK's max-text-values-per-UC limit

CREATE OR REPLACE FUNCTION check_max_text_per_fk_per_uc() RETURNS TRIGGER AS $$
DECLARE
    max_allowed field_key_type.max_text_per_uc%TYPE;
    found int;
BEGIN
    SELECT max_text_per_uc INTO max_allowed
    FROM field_key fk INNER JOIN field_key_type fkt ON type_id = fkt.id
    WHERE fk.id = NEW.fk_id;

    IF max_allowed IS NOT NULL THEN
        SELECT count(1) INTO found FROM text where uc_id = NEW.uc_id and fk_id = NEW.fk_id;
        IF found >= max_allowed THEN
            RAISE EXCEPTION 'UC % already has % text values for fk %.', NEW.uc_id, found, NEW.fk_id;
        END IF;
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER enforce_max_text_per_fk_per_uc
BEFORE INSERT OR UPDATE OF uc_id,fk_id ON text
FOR EACH ROW EXECUTE PROCEDURE check_max_text_per_fk_per_uc();

------------------------------------------------------------------------------------------------------------------------
-- Trigger: Maintains usecase.latest_rev_id

CREATE OR REPLACE FUNCTION update_latest_usecase_rev_1() RETURNS TRIGGER AS $$
DECLARE
    latest_rev int;
BEGIN

    select max(rev) into latest_rev from usecase_rev where ident_id = OLD.ident_id and rev != OLD.rev;

    update usecase
    set latest_rev_id = (
        select case when latest_rev is null then -1
               else (select id from usecase_rev where ident_id = OLD.ident_id and rev = latest_rev)
               end
    )
    where id = OLD.ident_id;

    IF (TG_OP = 'DELETE') THEN RETURN OLD;
    ELSE RETURN NEW;
    END IF;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION update_latest_usecase_rev_2() RETURNS TRIGGER AS $$
DECLARE
    latest_rev int;
BEGIN

    select max(rev) into latest_rev from usecase_rev where ident_id = NEW.ident_id;

    update usecase
    set latest_rev_id = (
        select case when NEW.rev = latest_rev then NEW.id
               else (select id from usecase_rev where ident_id = NEW.ident_id and rev = latest_rev)
               end
    )
    where id = NEW.ident_id;

    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER latest_usecase_rev_trigger_1 BEFORE UPDATE OF ident_id     OR DELETE ON usecase_rev FOR EACH ROW EXECUTE PROCEDURE update_latest_usecase_rev_1();
CREATE TRIGGER latest_usecase_rev_trigger_2 AFTER  UPDATE OF ident_id,rev OR INSERT ON usecase_rev FOR EACH ROW EXECUTE PROCEDURE update_latest_usecase_rev_2();

------------------------------------------------------------------------------------------------------------------------
-- Trigger: uc_field integrity checks

CREATE OR REPLACE FUNCTION check_uc_field_integrity() RETURNS TRIGGER AS $$
DECLARE
    uc int;
    fk int;
    fk_max_text field_key_type.max_text_per_uc%TYPE;
    x int;
    y int;
BEGIN
    -- Check ucRev and textRev share the same UC
    select ident_id into uc from usecase_rev where id = NEW.uc_rev_id;
    select uc_id,fk_id into x,fk from text t, text_rev r where r.id = NEW.text_rev_id and r.ident_id=t.id;
    if x != uc then
        raise exception 'UC mismatch. UC rev % belongs to % while text % belongs to %.', NEW.uc_rev_id, uc, NEW.text_rev_id, x;
    end if;

    -- Check parent's UC and FK
    if NEW.parent_rev_id is not null then
        select uc_id,fk_id into x,y from text t, text_rev r where r.id = NEW.parent_rev_id and r.ident_id=t.id;
        if x != uc then
            raise exception 'UC mismatch. UC rev % belongs to % while parent-text % belongs to %.', NEW.uc_rev_id, uc, NEW.parent_rev_id, x;
        end if;
        if y != fk then
            raise exception 'FK mismatch. Text % belongs to % while parent-text % belongs to %.', NEW.text_rev_id, fk, NEW.parent_rev_id, y;
        end if;
    end if;

    select max_text_per_uc into fk_max_text from field_key f, field_key_type t where type_id = t.id and f.id = fk;
    if NEW.label is null then
        -- FK must be text
        if fk_max_text is null then
            raise exception 'Text field value expected. Got % of FK %.', NEW.text_rev_id, fk;
        end if;
    else
        -- FK must be step
        if fk_max_text is not null then
            raise exception 'Step expected. A label cannot be applied to % of FK %.', NEW.text_rev_id, fk;
        end if;
    end if;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER enforce_uc_field_integrity
BEFORE INSERT OR UPDATE ON uc_field
FOR EACH ROW EXECUTE PROCEDURE check_uc_field_integrity();
