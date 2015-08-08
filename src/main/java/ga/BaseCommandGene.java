package ga;

import org.jgap.*;

public abstract class BaseCommandGene extends BaseGene {

    public BaseCommandGene(Configuration a_configuration) throws InvalidConfigurationException {
        super(a_configuration);
    }

    public abstract int doCompare(Object o);
    public abstract int doHashCode();
    public abstract boolean doEquals(Object o);

    public int compareTo(Object o) {
        return doCompare(o);
    }

    public int hashCode() {
        return doHashCode();
    }

    public boolean equals(Object o) {
        return doEquals(o);
    }

}