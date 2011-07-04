

all: figs
	

figs: srcs/iaf_component.svg srcs/coba_component.svg srcs/iaf_coba2_component_simplified.svg
	mkdir build
	convert -depth 16 -background transparent -resize 35% -density 300 srcs/iaf_component.svg build/iaf_component_im.png
	convert -depth 16 -background transparent -resize 35% -density 300 srcs/coba_component.svg build/coba_component_im.png
	
	convert -depth 16 -background transparent -resize 35% -density 300 srcs/iaf_coba2_component_simplified.svg build/iaf_coba2_component_im.png
	
	convert -depth 16 -background transparent -resize 15% -density 300 srcs/AL_UL_Overview.svg build/AL_UL_Overview.png

clean:
	rm -rf build/
