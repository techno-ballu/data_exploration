// Put code in an Immediately Invoked Function Expression (IIFE).
// This isn't strictly necessary, but it's good JavaScript hygiene.
(function() {

$(document).on("click", "button.custom-button", function(evt) {
  debugger
  // evt.target is the button that was clicked
  var el = $(evt.target);

  // Set the button's text to its current value plus 1
  // el.text(parseInt(el.text()) + 1);

  // Raise an event to signal that the value changed
  // el.trigger("change");
  var result = {
    id : el.data("feature"),
    class : $(el).attr("class")
  }
  Shiny.onInputChange(el.data("event"),result)
});

var button_binding = new Shiny.InputBinding();
$.extend(button_binding, {

  find: function(scope) {
    return $(scope).find("button#transform-categories");
  },

  getValue: function(el) {
    return $(el).text();
  },

  setValue: function(el, value) {
    $(el).text(value);
  },

  subscribe: function(el, callback) {
    // debugger
    $(el).on("change.button_binding", function(e) {
      callback();
    });
  },

  unsubscribe: function(el) {
    debugger
    $(el).off(".button_binding");
  },

  // Receive messages from the server.
  // Messages sent by updateButtonInput() are received by this function.
  receiveMessage: function(el, data) {
    if (data.hasOwnProperty('feature'))
      $(el).data('feature', data.feature);

    // $(el).trigger('change');
  }


});

Shiny.inputBindings.register(button_binding);

// URL input binding
// This input binding is very similar to textInputBinding from
// shiny.js.
// var urlInputBinding = new Shiny.InputBinding();


// // An input binding must implement these methods
// $.extend(urlInputBinding, {

//   // This returns a jQuery object with the DOM element
//   find: function(scope) {
//     debugger
//     return $(scope).find('input[type="url"]');
//   },

//   // return the ID of the DOM element
//   getId: function(el) {
//     return el.id;
//   },

//   // Given the DOM element for the input, return the value
//   getValue: function(el) {
//     return el.value;
//   },

//   // Given the DOM element for the input, set the value
//   setValue: function(el, value) {
//     el.value = value;
//   },

//   // Set up the event listeners so that interactions with the
//   // input will result in data being sent to server.
//   // callback is a function that queues data to be sent to
//   // the server.
//   subscribe: function(el, callback) {
//     debugger
//     $(el).on('keyup.urlInputBinding input.urlInputBinding', function(event) {
//       callback(true);
//       // When called with true, it will use the rate policy,
//       // which in this case is to debounce at 500ms.
//     });
//     $(el).on('change.urlInputBinding', function(event) {
//       callback(false);
//       // When called with false, it will NOT use the rate policy,
//       // so changes will be sent immediately
//     });
//   },

//   // Remove the event listeners
//   unsubscribe: function(el) {
//     $(el).off('.urlInputBinding');
//   },

//   // Receive messages from the server.
//   // Messages sent by updateUrlInput() are received by this function.
//   receiveMessage: function(el, data) {
//     if (data.hasOwnProperty('value'))
//       this.setValue(el, data.value);

//     if (data.hasOwnProperty('label'))
//       $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

//     $(el).trigger('change');
//   },

//   // This returns a full description of the input's state.
//   // Note that some inputs may be too complex for a full description of the
//   // state to be feasible.
//   getState: function(el) {
//     return {
//       label: $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(),
//       value: el.value
//     };
//   },

//   // The input rate limiting policy
//   getRatePolicy: function() {
//     return {
//       // Can be 'debounce' or 'throttle'
//       policy: 'debounce',
//       delay: 500
//     };
//   }
// });

// Shiny.inputBindings.register(urlInputBinding, 'shiny.urlInput');

})();


