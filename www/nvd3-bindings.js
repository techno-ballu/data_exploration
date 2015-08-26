// Put code in an Immediately Invoked Function Expression (IIFE).
// This isn't strictly necessary, but it's good JavaScript hygiene.



(function() {

var NO_TICKS = 0
// See http://rstudio.github.io/shiny/tutorial/#building-outputs for
// more information on creating output bindings.

// Generic output binding for NVD3 multi-bar chart
var nvd3_multibar_out_bind = new Shiny.OutputBinding();

nvd3_multibar_out_bind.find = function(scope) {
  // For the given scope, return the set of elements that belong to
  // this binding.
  return $(scope).find(".nvd3-histogram");
};

nvd3_multibar_out_bind.renderValue = function(el, data) {
  // This function will be called every time we receive new output
  // values for a line chart from Shiny. The "el" argument is the
  // div for this particular chart.
  // debugger
  var $el = $(el);
  var featureID = el.id;

  var tooltip = function(key, x, y, e, graph) {
      return '<h3>' + y + '</h3>' +
             '<p> on ' +  x + '</p>';
           };
    
  // The first time we render a value for a particular element, we
  // need to initialize the nvd3 line chart and d3 selection. We'll
  // store these on $el as a data value called "state".
  if (!$el.data("state")) {
    // Shiny.onInputChange("tooltip", "Hover over a bar to look at the details");
    var chart = nv.models.multiBarChart()
        .staggerLabels(false)    //Too many bars and not enough room? Try staggering labels.
        .showLegend(false)
        .showControls(false);
       

     if(data.showVerticalXaLabel){  // if block to switch off xticks in nero zero variance Tab 
    chart.xAxis.tickValues(NO_TICKS)
    }else{
       chart.reduceXTicks(true)
    }
    
    chart.margin({top: 40, right: 20, bottom: 30, left: 50})    

    // chart.xAxis.axisLabel("x axis");
    // chart.yAxis.axisLabel("y axis").axisLabelDistance(40);

    if (data.xlabel) {
      chart.xAxis.axisLabel(data.xlabel);
      chart.margin({top: 50, right: 20, bottom: 40, left: 50})
      //chart.xAxis.rotateLabels(-45)  // for rotation
    }  
    if ($.isNumeric(data.data[0].values[0].x)) {
      chart.xAxis
              .tickFormat(d3.format(',.1f'));
    } else
      chart.groupSpacing(0.5)

    if (data.ylabel)
       if(data.showVerticalXaLabel){  // if block to switch the chart margins for showing large xLabels in nero zero variance Tab 
      chart.yAxis.axisLabel(data.ylabel);
      chart.margin().left = 60
      }else{
        chart.yAxis.axisLabel(data.ylabel).axisLabelDistance(40);
      }

    if(data.showDecimal){
    chart.yAxis
        .tickFormat(d3.format(',.4f'));
    }else{
       chart.yAxis
        .tickFormat(d3.format(',f'));
    }

    nv.utils.windowResize(chart.update);
    
    var parent = d3.select(el);
    var selection = d3.select(el).select("svg");
    
    // Store the chart object on el so we can get it next time
    $el.data("state", {
      chart: chart,
      selection: selection,
      parent: parent
    });
  }
  
  // Now, the code that'll run every time a value is rendered...
  
  // Retrieve the chart and selection we created earlier
  var state = $el.data("state");
  
  // Schedule some work with nvd3
  nv.addGraph(function() {

    if (data.tooltip)
    tooltip = function(key, x, y, e, graph) {
      if (data.d) {
        // if (data.evt === "xoutliers" || data.evt === null) {
        
          if (!data.xlabel) {
          // debugger
          // console.log(data.d)
          indx = e.pointIndex
          var high = data.max.toFixed(1)
          if ((indx+1) !== e.series.values.length)
            high = e.series.values[indx+1].x.toFixed(1)

          splits = data.tooltip.replace('y',y).replace('low',x).replace('x1',+data.d[indx].toFixed(1))
          .replace('high',high).replace('x2',+data.d[indx+1].toFixed(1));
        } else {
          var txt = data.d[parseFloat(x)]
          if (Array.isArray(txt)) {
            array = txt;
            var texts = []
            var i,j,temparray,chunk = 10;
            for (i=0,j=array.length; i<j; i+=chunk)
              texts.push(array.slice(i,i+chunk).join());
            txt = texts.join("<br>")
          }
          
          // indx = e.pointIndex
           //splits = data.tooltip.replace('y',y).replace('x',x).replace('text',txt).replace('z',data.data[0].values[indx].y);  
          
        /*  splits = data.tooltip.replace('y',y).replace('x',x).replace('text',txt);
          if(decimal){
          indx = e.pointIndex
          splits = data.tooltip.replace('y',y).replace('x',x).replace('text',txt).replace('z',data.data[0].values[indx].y);  
          }*/
          
          indx = e.pointIndex
          if(data.showDecimal){   
           splits = data.tooltip.replace('y',y).replace('x',x).replace('text',txt).replace('z',data.data[0].values[indx].y);
          }else{
            splits = data.tooltip.replace('y',y).replace('x',x).replace('text',txt);
          }
          
          
        }
        // Shiny.onInputChange("tooltip", txt);
      } else {
        var x = x;
        if (!x)
          x = "empty string/s"
        splits = data.tooltip.replace('y',y).replace('x',x);
      }
      
      return splits;
    };

    state.chart.tooltip(tooltip);

    state.chart.multibar.dispatch.on("elementClick", function(e) {
        // console.log(data.d)
        if (!data.xlabel) {
          if ($.isNumeric(data.data[0].values[0].x)) {
            indx = e.pointIndex
            low = data.data[0].values[indx].x
            high = data.max
            if ((indx+1) !== e.series.values.length)
              high = data.data[0].values[indx+1].x
            result = {
              featureID : featureID,
              barNos : low + "," + high,
              value : e.value,
              random : Math.random()
            }
          } else {
            result = {
              featureID : featureID,
              barNos : e.point.x
            }
          }
        } else {
          var txts = data.d[e.point.x]
          var clickables = [];
          if (Array.isArray(txts))
            clickables = clickables.concat(txts);
          else
            clickables.push(txts)
          result = {
            y : e.point.y,
            x : e.point.x,
            featureID : featureID,
            barNos : clickables.join(),
            random : Math.random()
          }
        }

        Shiny.onInputChange(data.evt, result);
    });

    // Update the chart
    state.selection
      .datum(data.data)
      .transition(200)
      .call(state.chart);
       
        
    // var start;
    // var target;
    // var drag_behavior = d3.behavior.drag().origin(
    //   function(d) {
    //     t = d3.transform(d3.select(this).attr("transform"));
    //     start = {
    //       x : t.translate[0],
    //       y : t.translate[1]
    //     }; 
    //     return start;
    //   }).on("dragstart", function(d) {
    //   var self = d;
    //   d3.select(this).classed("dragging", true);
    //   $(this).insertBefore($(this.parentElement.children[0]))
    //   d3.selectAll(".nv-bar").on("mouseover", function(d) {
    //     if (d.x === self.x) {
    //       d3.event.stopPropagation();
    //       return false;
    //     }
    //     target = d.x;
    //     d3.select(this).style("stroke-width",2).style("stroke","black").style("stroke-opacity",1)
    //     // cancel the drag event
    //     // drag_behavior.on("drag", null);
    //   });
    //   d3.selectAll(".nv-bar").on("mouseout", function(d) {
    //     if (d.x === self.x) {
    //       d3.event.stopPropagation();
    //       return false;
    //     }
    //     d3.select(this).style("stroke-opacity",0)
    //     // cancel the drag event
    //     // drag_behavior.on("drag", null);
    //   });
    // }).on("drag", function(d) {
    //   var evtX = d3.event.x; // .duration(100)
    //   var evtY = d3.event.y;
    //   d3.mouse
    //   d3.select(this).transition().ease("elastic").attr("transform", function(d) {
    //     return "translate(" + evtX + "," + evtY + ")"
    //   })
    // }).on("dragend", function(d) {
    //   d3.select(this).transition().ease("bounce").attr("transform", function(d) {
    //     return "translate(" + start.x + "," + start.y + ")"
    //   })
    //   d3.selectAll(".nv-bar").on("mouseover", null);
    //   d3.selectAll(".nv-bar").on("mouseout", null);
    //   Shiny.onInputChange("mergeBar", {
    //     dragged : d.x,
    //     target : target
    //   })
    // })

    // d3.selectAll(".nv-bar").attr("cursor","move").call(drag_behavior);

    return state.chart;
  });
};

// Tell Shiny about the output binding
Shiny.outputBindings.register(nvd3_multibar_out_bind, "nvd3-multibar-out");

// Generic output binding for NVD3 horizontal-bar chart
var nvd3_horizontalbar_out_bind = new Shiny.OutputBinding();

nvd3_horizontalbar_out_bind.find = function(scope) {
  // For the given scope, return the set of elements that belong to
  // this binding.
  return $(scope).find(".nvd3-categories");
};

nvd3_horizontalbar_out_bind.renderValue = function(el, data) {
  // This function will be called every time we receive new output
  // values for a line chart from Shiny. The "el" argument is the
  // div for this particular chart.
  
  var $el = $(el);
  var featureID = el.id;

  var tooltip = function(key, x, y, e, graph) {
      return '<h3>' + y + '</h3>' +
             '<p> on ' +  x + '</p>';
  };
    
  // The first time we render a value for a particular element, we
  // need to initialize the nvd3 line chart and d3 selection. We'll
  // store these on $el as a data value called "state".
  if (!$el.data("state")) {
    var chart = nv.models.multiBarHorizontalChart()
      .x(function(d) { return d.label })
      .y(function(d) { return d.value })
      .margin({top: 10, right: 10, bottom: 20, left: 300})
      .showLegend(false)
      .showValues(true)
      .tooltips(false)
      .showControls(false);

    chart.yAxis
      .tickFormat(d3.format(',.2f'));

    chart.multibar.dispatch.on("elementClick", function(e) {
        result = {
          featureID : featureID,
          barNo : e.point.x
        }
        Shiny.onInputChange("mydata", result);
    });

    nv.utils.windowResize(chart.update);
    
    var selection = d3.select(el).select("svg");
    
    // Store the chart object on el so we can get it next time
    $el.data("state", {
      chart: chart,
      selection: selection
    });
  }
  
  // Now, the code that'll run every time a value is rendered...
  
  // Retrieve the chart and selection we created earlier
  var state = $el.data("state");
  
  // Schedule some work with nvd3
  nv.addGraph(function() {
    // Update the chart
    state.selection
      .datum(data)
      .transition(200)
      .call(state.chart);
      
   // addDwnloadButtonsInSVG(null, state.selection.node());  
    return state.chart;
  });
};

// Tell Shiny about the output binding
Shiny.outputBindings.register(nvd3_horizontalbar_out_bind, "nvd3-horizontalbar-out");

// Generic output binding for NVD3 scatter chart
var nvd3_scatter_bind = new Shiny.OutputBinding();

nvd3_scatter_bind.find = function(scope) {
  // For the given scope, return the set of elements that belong to
  // this binding.
  return $(scope).find(".nvd3-scatterchart");
};

nvd3_scatter_bind.renderValue = function(el, data) {
   // This function will be called every time we receive new output
  // values for a line chart from Shiny. The "el" argument is the
  // div for this particular chart.
     
     var $el = $(el);
   
    var selection = d3.select(el).select("svg");
    
    var svg = selection[0][0].innerHTML;
    
   // The first time we render a value for a particular element, we
   // need to initialize the nvd3 line chart and d3 selection. We'll
   // store these on $el as a data value called "state".
   if (!$el.data("state")) { 
   
    var chart = nv.models.scatterChart()
                .showDistX(false).showDistY(false)
                .showLegend(false).sizeRange([100, 150]);
    
    chart.margin({top: 30, right: 30, bottom: 50, left: 90}) 
    chart.xAxis.axisLabel(data.xlab).tickFormat(d3.format('.1f'));
    chart.yAxis.axisLabel(data.ylab).axisLabelDistance(30).tickFormat(d3.format('.1f')); //
    
    //We want to show shapes other than circles.
    chart.scatter.onlyCircles(true);
    nv.utils.windowResize(chart.update);
    var parent = d3.select(el);
    // Store the chart object on el so we can get it next time
    $el.data("state", {
      chart: chart,
      selection: selection,
      parent : parent
    });
   }
  
   // Retrieve the chart and selection we created earlier
   var state = $el.data("state");
    nv.addGraph(function() {
     state.chart.tooltipContent(function(key, x, y, obj) {
       return '<h5>' + data.xlab + ' : </h5><label>' +  x +  '</label>' + 
              '<h5>' +data.ylab + ' : </h5><label>' +  y +  '</label>'; 
     });

    state.chart.xAxis.axisLabel(data.xlab).tickFormat(d3.format('.1f'));
    state.chart.yAxis.axisLabel(data.ylab).tickFormat(d3.format('.1f'));
    // Update the chart
    state.selection
      .datum(data.data)
      .call(state.chart);
    return state.chart;
  });
  
};

// Tell Shiny about our new output binding
Shiny.outputBindings.register(nvd3_scatter_bind, "shinyjsexamples.nvd3-scatterchart");

//First create a generic output binding instance, then overwrite
//specific methods whose behavior we want to change.
var nvd3_linechart_bind = new Shiny.OutputBinding();

nvd3_linechart_bind.find = function(scope) {
// For the given scope, return the set of elements that belong to
// this binding.
return $(scope).find(".nvd3-linechart");
};

nvd3_linechart_bind.renderValue = function(el, data) {
// This function will be called every time we receive new output
// values for a line chart from Shiny. The "el" argument is the
// div for this particular chart.
// debugger

// if (data === null) {
//   return;
// }

// console.log([data]);
// data = [data]
var featureID = el.id;

var $el = $(el);
 
// The first time we render a value for a particular element, we
// need to initialize the nvd3 line chart and d3 selection. We'll
// store these on $el as a data value called "state".
if (!$el.data("state")) {
 var chart = nv.models.lineChart()
   // .margin({left: 100})
   .useInteractiveGuideline(false)
   .transitionDuration(350)
   .showLegend(true)
   .showYAxis(true)
   .showXAxis(true)
   .useVoronoi(false);
   
 chart.margin({top: 20, right: 30, bottom: 50, left: 70})   

 if (data.xlabel)
   chart.xAxis.axisLabel(data.xlabel);

 // chart.xAxis     //Chart x-axis settings
 //   .axisLabel('Time (ms)')
 //   .tickFormat(d3.format(',r'));

 chart.yAxis     //Chart y-axis settings
   .axisLabel(data.ylabel)
   .tickFormat(d3.format('.1f'));

 nv.utils.windowResize(chart.update);
 
 var selection = d3.select(el).select("svg");
 
 // Store the chart object on el so we can get it next time
 $el.data("state", {
   chart: chart,
   selection: selection
 });
}

// Now, the code that'll run every time a value is rendered...

// Retrieve the chart and selection we created earlier
var state = $el.data("state");

// Schedule some work with nvd3
nv.addGraph(function() {
 if (data.tooltip) {
   tooltip = function(key, x, y, e, graph) {
     // console.log(data.evt)

     if (data.d) {
       if (data.evt === "xpairswise") {
         splits = data.tooltip.replace('y',y).replace('n',data.d[key]).replace('key',key);
       } 
     }
     return splits;
   };
   state.chart.tooltipContent(tooltip);
 }
 

 state.chart.lines.dispatch.on("elementClick", function(e) {
     // debugger
     // Shiny.onInputChange("click", e.seriesIndex);
     Shiny.onInputChange(data.evt, e.series.key);
 });

 if (data.data.length === 0)
   state.selection.node().innerHTML = ""

 // Update the chart
 state.selection
   .datum(data.data)
   .transition(500)
   .call(state.chart);
 return state.chart;
});
};

//Tell Shiny about our new output binding
Shiny.outputBindings.register(nvd3_linechart_bind, "nvd3-line-out");
})();


