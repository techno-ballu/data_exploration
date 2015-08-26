(function() {
  
      /**
      * Saves a chart svg
      **/
      var saveSvgModule = (function(){
        var doctype = '<?xml version="1.0" standalone="no"?><!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www .w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">';
        
        var xmlns = "http://www.w3.org/2000/xmlns/";
        var version = "1.1";
        
        //creates a clone of html
        cloneEl = function(el){
           return el.cloneNode(true);
        };
        
        //adds attributes to a html tag
        addAtrributes = function(clone, h, w){
          clone.setAttribute("version", version);
          clone.setAttributeNS(xmlns, "xmlns", "http://www.w3.org/2000/svg");
          clone.setAttributeNS(xmlns, "xmlns:xlink", "http://www.w3.org/1999/xlink");
          clone.setAttribute("height", h);
          clone.setAttribute("width", w);
          return clone;
        };
        
        //creates a wrapper div to get the innerHTML
        createOuter = function(children){
          var outer = document.createElement("div");
          outer.appendChild(children);
          return outer;
        }
        
        //gets the svg content
        getSvg = function(el, h, w){
          var clonedEl = cloneEl(el);
          clonedEl.style.backgroundColor = 'white';
          clonedEl = addAtrributes(clonedEl, h, w);
          clonedEl = createStyles(clonedEl);
          var outer = createOuter(clonedEl);
          return doctype + outer.innerHTML;
         }
        
        //appends the stylesheets in a page
        createStyles = function(clone){
          var sheets = document.styleSheets;
          var used = "";
          for (var i = 0; i < sheets.length; i++) {
              var rules = sheets[i].cssRules;
              for (var j = 0; j < rules.length; j++) {
                var rule = rules[j];
                if (typeof(rule.style) != "undefined") {
                    var elems = document.querySelectorAll(rule.selectorText);
                    if (elems.length > 0) {
                      used += rule.selectorText + " { " + rule.style.cssText + " }\n";
                    }
                }
              }
            }
              
            var s = document.createElement('style');
            s.setAttribute('type', 'text/css');
            s.innerHTML = "<![CDATA[\n" + used + "\n]]>";
              
            var defs = document.createElement('defs');
            defs.appendChild(s);
            clone.insertBefore(defs, clone.firstChild);
            return clone;
        }
        
        //converts a html input to encoded uri
        svgAsDataUri = function(svg){
          var uri = 'data:image/svg+xml;base64,'+ btoa(unescape(encodeURIComponent(svg)));
          return uri;
        }
        
        //saves a chart as PNG
        saveSvgAsPng = function(uri, chartname){
          var image = new Image();
          image.src = uri;
          image.onload = function() {
             var canvas = document.createElement('canvas');
             var w = document.createAttribute("width");
             w.value = image.width;
             canvas.setAttributeNode(w);
             var h = document.createAttribute("height");
             h.value = image.height;
             canvas.setAttributeNode(h);
             
             var context = canvas.getContext('2d');
             context.drawImage(image, 0, 0);
      
             var a = document.createElement('a');
             a.download = chartname +  ".png";
             a.href = canvas.toDataURL('image/png')
             document.body.appendChild(a);
             a.click();
          }
        }
        
        return {
           saveChart : function(chartname, el, h, w){
            var svg = getSvg(el, h, w);
            var uri = svgAsDataUri(svg);
            saveSvgAsPng(uri, chartname);
          },
          saveSvgAsPng : function(uri, chartname){
            saveSvgAsPng(uri, chartname);
          }
        };
        
      })();

    /**
     * Wrapper function to save a chart
     * */
    function saveChart(chartname, el, type){
      var w;
      var h;
      switch(type){
       case 'clean_row':
       case 'clean_col':
       case 'histogram':
       case 'scatter':
        w = getComputedStyle(el).getPropertyValue('width');
        h = getComputedStyle(el).getPropertyValue('height');
        saveSvgModule.saveChart(chartname, el, h, w);
       break;
       case 'box':
         saveSvgModule.saveSvgAsPng(el.src, chartname);
      break;
     }
    }
  
   $(document).on("click", "#save", function() {
     var parent = $(this).parent();
     var pid =parent[0].id;
     var chartname;
     var type = pid.split("-")[0];
     var chartParent = parent.next()[0];
     var chart;
     switch(type){
       case 'clean_row':
       case 'clean_col':
       case 'histogram':
       chartname = chartParent.id;
       chart = d3.select(chartParent).select("svg").node();
       break;
       case 'box':
         chartname = chartParent.id;
         chart = d3.select(chartParent).select("img").node();
         break;
       case 'scatter':
        chartname = $("#x")[0].value + "_" + $("#y")[0].value;
        chart = d3.select(chartParent).select("svg").node();
      }
     saveChart(chartname, chart, type);
   });
   
   $(document).on("click", "#export", function() {
     var parent = $(this).parent();
     var pid =parent[0].id;
     var type = pid.split("-")[0];
     var features = {};
     var inputs = {
       type : type,
       random  : Math.random() 
     };
     switch(type){
       case 'clean_row':
         break;
       case 'clean_col':
         break;
       case 'histogram':
         var numerical = parent.next()[0].id.split('-')[1];
         features.x = numerical;
         break;
       case 'box':
         var id = parent.next()[0].id;
         var idx = id.indexOf('_');
         var numerical = id.substr(idx + 1)
         features.x = numerical;
         break;
       case 'scatter':
         features.x = $("#x")[0].value;
         features.y = $("#y")[0].value;
         break;
      }
      inputs.features = features;
      Shiny.onInputChange('export', inputs)
     });
})();  