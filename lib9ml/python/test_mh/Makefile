

clean:
	rm -rf coverage_htmlout/ .coverage output/ nineml_mechanisms/

coverage:
	coverage run  --source=nineml functional/round_trip_xml_testing/test_components.py
	nosetests --with-coverage --cover-package=nineml  unit/nineml_test/
	coverage html -i -d coverage_htmlout --include="*nineml*"
