// Enhanced Inputs - Custom Bindings and Reset Functionality

// ============================================================================
// Enhanced Numeric Input - Custom Shiny Binding
// ============================================================================

var enhancedNumericBinding = new Shiny.InputBinding();

$.extend(enhancedNumericBinding, {
  find: function(scope) {
    return $(scope).find('input.enhanced-numeric-input');
  },
  
  getId: function(el) {
    return el.id;
  },
  
  getValue: function(el) {
    var $el = $(el);
    var val = $el.val();
    
    // Empty value
    if (val === '' || val === null || val === undefined) {
      return null;
    }
    
    var numVal = parseFloat(val);
    
    // Not a valid number
    if (isNaN(numVal)) {
      return null;
    }
    
    // Check min/max
    var min = $el.attr('min');
    var max = $el.attr('max');
    
    if (min !== undefined && min !== '') {
      if (numVal < parseFloat(min)) {
        return null;
      }
    }
    
    if (max !== undefined && max !== '') {
      if (numVal > parseFloat(max)) {
        return null;
      }
    }
    
    // Valid value
    return numVal;
  },
  
  setValue: function(el, value) {
    var $el = $(el);
    if (value === null || value === undefined) {
      $el.val('');
    } else {
      $el.val(value);
    }
    updateEnhancedNumericState($el);
  },
  
  subscribe: function(el, callback) {
    var $el = $(el);
    
    // Initial state
    updateEnhancedNumericState($el);
    
    // Update on input (as user types)
    $el.on('input.enhancedNumeric', function() {
      updateEnhancedNumericState($el);
      callback(true);
    });
    
    // Also on change (spinner, paste, etc.)
    $el.on('change.enhancedNumeric', function() {
      updateEnhancedNumericState($el);
      callback(true);
    });
  },
  
  unsubscribe: function(el) {
    $(el).off('.enhancedNumeric');
  },
  
  receiveMessage: function(el, data) {
    var $el = $(el);
    
    if (data.hasOwnProperty('value')) {
      this.setValue(el, data.value);
    }
    if (data.hasOwnProperty('min')) {
      $el.attr('min', data.min);
    }
    if (data.hasOwnProperty('max')) {
      $el.attr('max', data.max);
    }
    if (data.hasOwnProperty('step')) {
      $el.attr('step', data.step);
    }
    if (data.hasOwnProperty('placeholder')) {
      $el.attr('placeholder', data.placeholder);
    }
    
    updateEnhancedNumericState($el);
    $el.trigger('change');
  },
  
  getRatePolicy: function() {
    return {
      policy: 'debounce',
      delay: 250
    };
  }
});

Shiny.inputBindings.register(enhancedNumericBinding, 'enhanced.numericInput');


// Update visual state of enhanced numeric input
function updateEnhancedNumericState($input) {
  var val = $input.val();
  var isRequired = $input.data('required');
  
  // Remove all state classes
  $input.removeClass('enhanced-input-empty-required enhanced-input-empty-optional enhanced-input-invalid');
  
  // Empty value
  if (val === '' || val === null || val === undefined) {
    if (isRequired === true || isRequired === 'true') {
      $input.addClass('enhanced-input-empty-required');
    } else if (isRequired === false || isRequired === 'false') {
      $input.addClass('enhanced-input-empty-optional');
    }
    return;
  }
  
  var numVal = parseFloat(val);
  
  // Invalid number
  if (isNaN(numVal)) {
    $input.addClass('enhanced-input-invalid');
    return;
  }
  
  // Check min/max
  var min = $input.attr('min');
  var max = $input.attr('max');
  
  if (min !== undefined && min !== '') {
    if (numVal < parseFloat(min)) {
      $input.addClass('enhanced-input-invalid');
      return;
    }
  }
  
  if (max !== undefined && max !== '') {
    if (numVal > parseFloat(max)) {
      $input.addClass('enhanced-input-invalid');
      return;
    }
  }
  
  // Valid - no special class needed
}


// ============================================================================
// Reset Button Functionality
// ============================================================================

$(document).on('click', '.reset-icon', function(e) {
  e.preventDefault();
  $(this).blur();
  
  var inputId = $(this).data('input-id');
  var defaultValue = $(this).data('default-value');
  
  if (!inputId || defaultValue === undefined) return;
  
  // Parse the JSON-encoded default value
  try {
    defaultValue = JSON.parse(defaultValue);
  } catch (e) {
    // If not valid JSON, use as-is (string)
  }
  
  var $input = $('#' + CSS.escape(inputId));
  
  // Check if it's an enhanced numeric input
  if ($input.hasClass('enhanced-numeric-input')) {
    $input.val(defaultValue === null ? '' : defaultValue);
    updateEnhancedNumericState($input);
    $input.trigger('change');
    return;
  }
  
  // Otherwise, detect type and reset accordingly
  var $container = $input.closest('.shiny-input-container, .form-group, .shiny-input-radiogroup, .shiny-input-checkboxgroup');
  var inputType = detectInputType(inputId, $container);
  
  switch (inputType) {
    case 'numeric':
    case 'text':
      resetTextNumeric(inputId, defaultValue);
      break;
    case 'slider':
      resetSlider(inputId, defaultValue);
      break;
    case 'radio':
      resetRadio(inputId, defaultValue);
      break;
    case 'select':
    case 'selectize':
      resetSelect(inputId, defaultValue);
      break;
    case 'checkbox':
      resetCheckbox(inputId, defaultValue);
      break;
    default:
      resetTextNumeric(inputId, defaultValue);
  }
});

function detectInputType(inputId, $container) {
  var $input = $('#' + CSS.escape(inputId));
  
  if ($input.hasClass('js-range-slider') || $input.closest('.shiny-input-container').find('.js-range-slider').length) {
    return 'slider';
  }
  if ($input.attr('type') === 'number' || $input.closest('.shiny-input-container').hasClass('shiny-input-number')) {
    return 'numeric';
  }
  if ($input.is('select') || $container.find('select').length) {
    var $select = $input.is('select') ? $input : $container.find('select');
    return $select.hasClass('selectized') ? 'selectize' : 'select';
  }
  if ($container.hasClass('shiny-input-radiogroup') || $input.attr('type') === 'radio') {
    return 'radio';
  }
  if ($input.attr('type') === 'checkbox' || $input.hasClass('shiny-input-checkbox')) {
    return 'checkbox';
  }
  if ($input.attr('type') === 'text') {
    return 'text';
  }
  
  return 'unknown';
}

function resetTextNumeric(inputId, value) {
  var $input = $('#' + CSS.escape(inputId));
  $input.val(value).trigger('change');
  Shiny.setInputValue(inputId, value);
}

function resetSlider(inputId, value) {
  var $slider = $('#' + CSS.escape(inputId));
  
  if (Array.isArray(value)) {
    $slider.data('ionRangeSlider').update({ from: value[0], to: value[1] });
  } else {
    $slider.data('ionRangeSlider').update({ from: value });
  }
  
  Shiny.setInputValue(inputId, value);
}

function resetRadio(inputId, value) {
  var $container = $('#' + CSS.escape(inputId));
  
  $container.find('input[type="radio"]').prop('checked', false);
  $container.find('input[type="radio"][value="' + CSS.escape(value) + '"]').prop('checked', true);
  
  Shiny.setInputValue(inputId, value);
}

function resetSelect(inputId, value) {
  var $select = $('#' + CSS.escape(inputId));
  
  if ($select.hasClass('selectized') || $select.next('.selectize-control').length) {
    var selectize = $select[0].selectize;
    if (selectize) {
      selectize.setValue(value, true);
    }
  } else {
    $select.val(value).trigger('change');
  }
  
  Shiny.setInputValue(inputId, value);
}

function resetCheckbox(inputId, value) {
  var $input = $('#' + CSS.escape(inputId));
  $input.prop('checked', value).trigger('change');
  Shiny.setInputValue(inputId, value);
}
