CREATE TABLE public.time_series_mapping
(
    uuid uuid PRIMARY KEY,
    participant uuid,
    time_series uuid
)
    WITHOUT OIDS
    TABLESPACE pg_default;

INSERT INTO
    public.time_series_mapping (uuid, participant, time_series)
VALUES
('58167015-d760-4f90-8109-f2ebd94cda91', 'b86e95b0-e579-4a80-a534-37c7a470a409', '9185b8c1-86ba-4a16-8dea-5ac898e8caa5'),
('9a9ebfda-dc26-4a40-b9ca-25cd42f6cc3f', 'c7ebcc6c-55fc-479b-aa6b-6fa82ccac6b8', '3fbfaa97-cff4-46d4-95ba-a95665e87c26'),
('9c1c53ea-e575-41a2-a373-a8b2d3ed2c39', '90a96daa-012b-4fea-82dc-24ba7a7ab81c', '3fbfaa97-cff4-46d4-95ba-a95665e87c26');
