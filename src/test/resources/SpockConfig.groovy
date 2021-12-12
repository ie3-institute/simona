import spock.lang.Specification

//improved logging for Spock tests, see https://github.com/spockframework/spock/issues/538#issuecomment-285936181 for details

class LabelPrinter {
    def _(def message) {
        println message
        true
    }
}

Specification.mixin LabelPrinter