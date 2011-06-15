
def dot_escape(s):

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



# From Regime, was a class method:
# def dot_content(self):
#
#    # template & namespace
#    ns = {}
#    t = '<tr><td align="left" port="n_%(node_id)s">%(node_content)s</td></tr>\\\n\t\t'
#
#    # header
#    contents = []
#
#    node_id = 0
#    for n in self.nodes:
#        # render node contents
#        ns['node_id'] = str(node_id)
#        node_id+=1
#        ns['node_content'] = dot_escape(n.as_expr())
#        contents += [t % ns]
#
#    return ''.join(contents)
#
#

# From Transition:

#def to_dot(self,out, show_contents=True, relabel_nodes=False):
#        """ Write a DOT graph representation of component
#
#        http://en.wikipedia.org/wiki/DOT_language
#
#        Convert a dot file to an image using, i.e.:
#          dot -Tsvg spike_generator.dot -o spike_generator.svg
#          dot -Tpng spike_generator.png -o spike_generator.png
#
#        """
#
#        # if out is a str, make a file
#        if isinstance(out,str):
#            out = file(out,'w')
#
#        out.write("""digraph "NineML Component '%s'" {\n""" % self.name)
#
#        out.write('\toverlap = "scale";\n')
#
#        regime_id = dict([(kv[0],i) for i,kv in enumerate(self.regime_map.iteritems())])
#
#        #TransitionLabelDict:
#        node_labels = {}
#        for r in itertools.chain(self.regimes):
#            node_labels[r] = "Regime:%d"%len(node_labels)
#
#        for t in itertools.chain(self.transitions):
#            node_labels[t] = "Trans:%d"%len(node_labels)
#
#
#        if show_contents:
#
#            out.write('\tgraph [fontsize=30 labelloc="t" label="" splines=true overlap=false rankdir = "LR"];\n\tratio = auto\n');
#            props = 'style = "filled, bold" penwidth = 1 fillcolor = "white" fontname = "Courier New" shape = "Mrecord" '
#            # regime template
#            t_regime = '\t"%(node)s" [ style = "filled, bold" penwidth = 1 fillcolor = "white" fontname = "Courier New" '+\
#                       'shape = "Mrecord" \\\n\t\tlabel =<<table border="0" cellborder="0" cellpadding="3" bgcolor="white">'+\
#                       '<tr><td bgcolor="black" \\\n\t\talign="center" colspan="2"><font color="white">'+\
#                       '%(regime_name)s</font></td></tr>\\\n\t\t%(contents)s</table>> ];\n ' 
#            # to fill: node, regime_name, contents
#            ns = {}
#
#            for r in self.regimes:
#                
#                ns['node'] = "regime_%d" % regime_id[r.name]
#                ns['regime_name'] = r.name if not relabel_nodes else node_labels[r]
#                ns['contents'] = r.dot_content()
#                out.write(t_regime % ns)
#
#
#
#        # transition template
#        t_transition = '\t"%(from)s" -> "%(to)s" [ style = "filled, bold" penwidth = 1 fillcolor = "white" fontname = "Courier New" \\\n'+\
#                       '\t\tlabel =<<table border="0" cellborder="0" cellpadding="3" bgcolor="#C0C0C0"> \\\n'+\
#                       '\t\t<tr><td bgcolor="blue" align="center" colspan="2"><font color="white"> \\\n'+\
#                       '\t\t%(trans_name)s</font> \\\n'+\
#                       '\t\t</td></tr> \\\n'+\
#                       '\t\t<tr><td bgcolor="green" align="center" colspan="2"><font color="black"> \\\n'+\
#                       '\t\t @ %(condition)s</font></td></tr> \\\n'+\
#                       '\t\t%(contents)s</table>> ];\n ' 
#        
#        for t in self.transitions:
#            if show_contents:
#
#                #out.write('label="%s @ %s"' % (t.name.encode('utf-8'), t.condition.encode('utf-8'),))
#                #out.write('\tregime_%d -> regime_%d [label="%s @ %s"];\n' % (regime_id[t.from_.name], regime_id[t.to.name],
#                #                                                   t.name.encode('utf-8'), t.condition.encode('utf-8')))
#
#                # to fill: node, regime_name, contents
#                ns = {}
#
#                ns['from'] = "regime_%d" % regime_id[t.from_.name]
#                ns['to'] = "regime_%d" % regime_id[t.to.name]
#                ns['trans_name'] = t.name if not relabel_nodes else node_labels[t]
#                ns['condition'] = dot_escape(t.condition.as_expr())
#                #ns['contents'] = t.dot_content()
#                ns['contents'] = t.dot_content()
#                out.write(t_transition % ns)
#                
#            else:
#                out.write('\tregime_%d -> regime_%d [label="%s"];\n' % (regime_id[t.from_.name], regime_id[t.to.name],
#                                                                   t.name.encode('utf-8')))
#
#        out.write('}')
