

import os
import re
from Cheetah.Template import Template

import nineml


def _dot_escape(s):

    dot_escape_table = {
    "&": "&amp;",
    '"': "&quot;",
    "'": "&apos;",
    ">": "&gt;",
    "<": "&lt;",
    "(": "&#40;",
    ")": "&#41;",
    }

    return "".join(dot_escape_table.get(c,c) for c in s)



_regime_node_tmpl_text = """

digraph G1 {

graph [fontsize=30 labelloc="t" label="NineML Component $component.name"
splines=true overlap=false rankdir = "LR" nodesep=2.0];
ratio = auto



#* Regimes *#
#for regime in $component.regimes:
$regime_node_names[regime] [ style = "filled, bold" 
                             penwidth = 1 
                             fillcolor = "white" 
                             fontname = "Courier New" 
                             shape = "Mrecord" 
   
    label =<<table border="0" cellborder="0" cellpadding="3" bgcolor="white"> 
            <tr>
              <td bgcolor="black" align="center" colspan="1">
                <font color="white"> 
                    $regime.name
                </font>
              </td>
            </tr>
            #for time_derivative in $regime.time_derivatives:
            <tr>
                <td align="left" >
                    $time_derivative.lhs = $time_derivative.rhs
                </td>
            </tr>
            #end for


            </table>> ];
#end for
#* End Regimes *#





#* Transitions *#

#for regime in $component.regimes:
    #for transition in $regime.transitions:

    $regime_node_names[transition.source_regime] -> $regime_node_names[transition.target_regime] 
        [ style = "filled, bold" 
          penwidth = 1 
          fillcolor = "white" 
          fontname = "Courier New" 
          label =<<table border="0" cellborder="0" cellpadding="3" bgcolor="#C0C0C0"> 
                   <tr>
                     <td bgcolor="blue" align="center" >
                        <font color="white"> 
                           Transition
                        </font> 
                     </td>
                   </tr> 
                   <tr>
                     <td bgcolor="green" align="center" >
                       <font color="black"> 
                           @ ${dot_escape(str($transition)) }
                       </font>
                      </td>
                   </tr> 

                 #for state_assignment in $transition.state_assignments:
                   <tr>
                    <td>Assign: $state_assignment.lhs &lt;= $state_assignment.rhs </td>
                   </tr>
                 #end for

                 #for event_output in $transition.event_outputs:
                 <tr>
                   <td>Emit Event: $event_output.port_name  </td>
                 </tr>
                 #end for
                </table>> ];

    #end for
#end for
#* EndTransitions *#





}
"""



class DotWriter(object):
    """Dot Writer docstring"""

    @classmethod
    def build(cls, dot_filename, output_types=None):
        """Runs the commandline tool, ``dot`` on a filename to produce output
        figures.

        :params dot_filename: The filename of the .dot file.
        :params output_types: The types of output that should be produced, by
            default 'pdf' will be produced. This should be a list, for example, 
            ['svg','pdf','png']
        
        """

        output_types = output_types or ['pdf']
        
        for t in output_types:
            t = t.replace('.','')
            f_out = dot_filename.replace('.dot','.'+t)
            exec_str = 'dot -T%s %s -o %s'%(t,dot_filename,f_out) 
            os.system(exec_str) 

    
    @classmethod
    def write(self, component, filename, flatten=True):
        """ Writes a component out to the .dot format"""
        

        if not component.is_flat() and flatten:
            
            component = nineml.al.flattening.flatten(component)



        regime_node_names = dict( (regime,'regime%d'%i) for i,regime in enumerate(component.regimes) )
        context = { 'component':component, 
                    'regime_node_names': regime_node_names, 
                    'dot_escape':_dot_escape}
        regime_text = Template(_regime_node_tmpl_text, context ).respond()

        
        # Remove Extra whitespace - otherwise we end
        # up with really long squares in the output
        p = re.compile(r'\s+')
        regime_text = p.sub(' ', regime_text)

        
        f = open(filename,'w')
        f.write( regime_text )
        f.close()











