
from base import ComponentValidatorPerNamespace
import nineml.utility

class ComponentValidatorDuplicateRegimeNames(ComponentValidatorPerNamespace):
    def __init__(self, component):
        ComponentValidatorPerNamespace.__init__(self, explicitly_require_action_overrides=False)
        self.visit(component)

    def action_componentclass(self,componentclass, namespace):
        regime_names = [r.name for r in componentclass.regimes]
        nineml.utility.assert_no_duplicates(regime_names)
