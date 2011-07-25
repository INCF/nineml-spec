

clean:
	rm -rf coverage_htmlout/ .coverage output/ nineml_mechanisms/

coverage:
	rm -rf coverage_htmlout/ .coverage output/ nineml_mechanisms/
	coverage run  --source='nineml' functional/round_trip_xml_testing/test_components.py
	nosetests --with-coverage --cover-package='nineml'  unit/nineml_test/
	#coverage html -i -d coverage_htmlout 
	coverage html -i -d coverage_htmlout --include="/home/hull/src/nineml-svn-model-tree/lib9ml/python/nineml*" --omit="/home/hull/src/nineml-svn-model-tree/lib9ml/python/nineml/user_layer.py"
