// Enhanced Inputs - Reset Functionality

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
  
  var $container = $('#' + CSS.escape(inputId)).closest('.shiny-input-container, .form-group, .shiny-input-radiogroup, .shiny-input-checkboxgroup');
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
      // Fallback: try generic approach
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
  
  // Handle both single value and range sliders
  if (Array.isArray(value)) {
    $slider.data('ionRangeSlider').update({ from: value[0], to: value[1] });
  } else {
    $slider.data('ionRangeSlider').update({ from: value });
  }
  
  Shiny.setInputValue(inputId, value);
}

function resetRadio(inputId, value) {
  var $container = $('#' + CSS.escape(inputId));
  
  // Uncheck all, then check the default
  $container.find('input[type="radio"]').prop('checked', false);
  $container.find('input[type="radio"][value="' + CSS.escape(value) + '"]').prop('checked', true);
  
  Shiny.setInputValue(inputId, value);
}

function resetSelect(inputId, value) {
  var $select = $('#' + CSS.escape(inputId));
  
  // Handle selectize differently
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
