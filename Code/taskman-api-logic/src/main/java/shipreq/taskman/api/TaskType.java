package shipreq.taskman.api;

// TODO Fuck this, just use a sealed trait myself and fail-fast on dup ids

public enum TaskType {

    RegistrationRequested(100),
    RegistrationCompleted(101),
    ReRegistrationAttempted(102),
    PasswordResetRequested(103),
    LandingPageHit(200);

    public final int id;
    private TaskType(int id) {
        this.id = id;
    }
}
