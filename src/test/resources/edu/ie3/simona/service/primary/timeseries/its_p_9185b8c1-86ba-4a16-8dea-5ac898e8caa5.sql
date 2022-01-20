CREATE TABLE public."its_p_9185b8c1-86ba-4a16-8dea-5ac898e8caa5"
(
    time timestamp with time zone,
    p double precision,
    uuid uuid,
    CONSTRAINT its_p_pkey PRIMARY KEY (uuid)
)
    WITH (
        OIDS = FALSE
    )
    TABLESPACE pg_default;

INSERT INTO
    public."its_p_9185b8c1-86ba-4a16-8dea-5ac898e8caa5" (uuid, time, p)
VALUES
('0245d599-9a5c-4c32-9613-5b755fac8ca0', '2020-01-01 00:00:00+0', 1000.0),
('a5e27652-9024-4a93-9d2a-590fbc3ab5a1', '2020-01-01 00:15:00+0', 1250.0);
