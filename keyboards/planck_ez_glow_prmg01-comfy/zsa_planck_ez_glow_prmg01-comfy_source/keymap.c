#include QMK_KEYBOARD_H
#ifdef AUDIO_ENABLE
#include "muse.h"
#endif
#include "eeprom.h"
#ifndef ZSA_SAFE_RANGE
#define ZSA_SAFE_RANGE SAFE_RANGE
#endif

enum custom_keycodes {
  RGB_SLD = ZSA_SAFE_RANGE,
};


enum planck_layers {
  _BASE,
  _LOWER,
  _RAISE,
  _ADJUST,
  _LAYER4,
  _LAYER5,
  _LAYER6,
  _LAYER7,
};

#define LOWER MO(_LOWER)
#define RAISE MO(_RAISE)

#define DUAL_FUNC_0 LT(13, KC_I)
#define DUAL_FUNC_1 LT(1, KC_F17)

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
  [_BASE] = LAYOUT_planck_grid(
    KC_ESCAPE,      KC_Q,           KC_W,           KC_E,           KC_R,           KC_T,           KC_Y,           KC_U,           KC_I,           KC_O,           KC_P,           KC_BSPC,        
    DUAL_FUNC_0,    KC_A,           KC_S,           KC_D,           KC_F,           KC_G,           KC_H,           KC_J,           KC_K,           KC_L,           KC_SCLN,        KC_ENTER,       
    KC_LEFT_SHIFT,  KC_Z,           KC_X,           KC_C,           KC_V,           KC_B,           KC_N,           KC_M,           KC_COMMA,       KC_DOT,         KC_SLASH,       KC_QUOTE,       
    KC_LEFT_CTRL,   KC_GRAVE,       KC_LEFT_GUI,    KC_LEFT_ALT,    LOWER,          KC_SPACE,       KC_NO,          KC_MINUS,       KC_EQUAL,       KC_BSLS,        KC_LBRC,        KC_RBRC
  ),

  [_LOWER] = LAYOUT_planck_grid(
    KC_GRAVE,       KC_1,           KC_2,           KC_3,           KC_4,           KC_5,           KC_6,           KC_7,           KC_8,           KC_9,           KC_0,           KC_TRANSPARENT, 
    KC_TRANSPARENT, KC_EXLM,        KC_AT,          KC_HASH,        KC_DLR,         KC_PERC,        KC_CIRC,        KC_4,           KC_5,           KC_6,           KC_DOT,         KC_TRANSPARENT, 
    KC_TRANSPARENT, KC_ESCAPE,      KC_ASTR,        KC_LPRN,        KC_RPRN,        KC_AUDIO_VOL_DOWN,KC_AUDIO_VOL_UP,KC_1,           KC_2,           KC_3,           KC_COMMA,       KC_DOT,         
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_AUDIO_MUTE,  KC_NO,          KC_MINUS,       KC_EQUAL,       KC_SLASH,       KC_ASTR,        KC_PLUS
  ),

  [_RAISE] = LAYOUT_planck_grid(
    KC_TRANSPARENT, KC_NO,          KC_NO,          KC_END,         KC_NO,          KC_NUM,         KC_SCRL,        KC_CAPS,        KC_INSERT,      KC_NO,          KC_UP,          KC_TRANSPARENT, 
    KC_TRANSPARENT, KC_HOME,        KC_NO,          KC_DELETE,      KC_RIGHT,       KC_NO,          KC_NO,          KC_F1,          KC_F2,          KC_F3,          KC_F4,          KC_TRANSPARENT, 
    KC_TRANSPARENT, KC_NO,          KC_NO,          KC_PAGE_UP,     KC_PGDN,        KC_LEFT,        KC_DOWN,        KC_F5,          KC_F6,          KC_F7,          KC_F8,          KC_NO,          
    KC_TRANSPARENT, KC_NO,          KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_NO,          KC_NO,          KC_F9,          KC_F10,         KC_F11,         KC_F12,         KC_NO
  ),

  [_ADJUST] = LAYOUT_planck_grid(
    KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          TOGGLE_LAYER_COLOR,RGB_TOG,        RGB_VAI,        
    KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          TO(5),          KC_NO,          KC_TRANSPARENT, RGB_SLD,        RGB_MODE_FORWARD,KC_NO,          RGB_VAD,        
    KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          TO(4),          RGB_SAD,        RGB_SAI,        KC_NO,          KC_NO,          
    KC_NO,          QK_BOOT,        KC_NO,          KC_NO,          KC_TRANSPARENT, KC_NO,          KC_NO,          RGB_SPD,        RGB_SPI,        KC_NO,          RGB_HUD,        RGB_HUI
  ),

  [_LAYER4] = LAYOUT_planck_grid(
    KC_ESCAPE,      KC_MS_BTN4,     KC_MS_BTN3,     KC_MS_BTN5,     KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_BSPC,        
    KC_TAB,         KC_MS_BTN1,     KC_NO,          KC_MS_BTN2,     KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_MS_BTN3,     KC_ENTER,       
    KC_LEFT_SHIFT,  KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_MS_WH_LEFT,  KC_MS_WH_RIGHT, KC_NO,          KC_NO,          KC_MS_BTN1,     KC_MS_UP,       KC_MS_BTN2,     
    KC_LEFT_CTRL,   TO(0),          KC_LEFT_GUI,    KC_LEFT_ALT,    KC_NO,          KC_MS_ACCEL2,   KC_NO,          KC_MS_WH_DOWN,  KC_MS_WH_UP,    KC_MS_LEFT,     KC_MS_DOWN,     KC_MS_RIGHT
  ),

  [_LAYER5] = LAYOUT_planck_grid(
    KC_ESCAPE,      KC_Q,           KC_W,           KC_E,           KC_R,           KC_T,           KC_Y,           KC_U,           KC_I,           KC_O,           KC_P,           KC_BSPC,        
    KC_TAB,         KC_A,           KC_S,           KC_D,           KC_F,           KC_G,           KC_H,           KC_J,           KC_K,           KC_L,           KC_SCLN,        KC_ENTER,       
    KC_LEFT_SHIFT,  KC_Z,           KC_X,           KC_C,           KC_V,           KC_B,           KC_N,           KC_M,           KC_COMMA,       KC_DOT,         KC_UP,          KC_QUOTE,       
    KC_LEFT_CTRL,   DUAL_FUNC_1,    KC_LEFT_GUI,    KC_LEFT_ALT,    MO(6),          KC_SPACE,       KC_NO,          KC_MINUS,       KC_EQUAL,       KC_LEFT,        KC_DOWN,        KC_RIGHT
  ),

  [_LAYER6] = LAYOUT_planck_grid(
    KC_GRAVE,       KC_1,           KC_2,           KC_3,           KC_4,           KC_5,           KC_6,           KC_7,           KC_8,           KC_9,           KC_0,           KC_DELETE,      
    KC_CAPS,        KC_F1,          KC_F2,          KC_F3,          KC_F4,          KC_F5,          KC_F6,          KC_F7,          KC_F8,          KC_F9,          KC_DOT,         KC_INSERT,      
    KC_LEFT_SHIFT,  KC_F10,         KC_F11,         KC_F12,         KC_NO,          KC_AUDIO_VOL_DOWN,KC_AUDIO_VOL_UP,KC_NO,          KC_NO,          KC_HOME,        KC_UP,          KC_END,         
    KC_LEFT_CTRL,   KC_TRANSPARENT, KC_LEFT_GUI,    KC_LEFT_ALT,    KC_TRANSPARENT, KC_AUDIO_MUTE,  KC_NO,          KC_PGDN,        KC_PAGE_UP,     KC_LEFT,        KC_DOWN,        KC_RIGHT
  ),

  [_LAYER7] = LAYOUT_planck_grid(
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, 
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, 
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, 
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_NO,          KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT
  ),

};

const uint16_t PROGMEM combo0[] = { KC_UP, KC_EQUAL, COMBO_END};
const uint16_t PROGMEM combo1[] = { KC_DOWN, KC_EQUAL, COMBO_END};
const uint16_t PROGMEM combo2[] = { KC_RIGHT, KC_EQUAL, COMBO_END};
const uint16_t PROGMEM combo3[] = { KC_EQUAL, KC_LEFT, COMBO_END};

combo_t key_combos[COMBO_COUNT] = {
    COMBO(combo0, KC_SLASH),
    COMBO(combo1, KC_LBRC),
    COMBO(combo2, KC_RBRC),
    COMBO(combo3, KC_BSLS),
};



extern rgb_config_t rgb_matrix_config;

RGB hsv_to_rgb_with_value(HSV hsv) {
  RGB rgb = hsv_to_rgb( hsv );
  float f = (float)rgb_matrix_config.hsv.v / UINT8_MAX;
  return (RGB){ f * rgb.r, f * rgb.g, f * rgb.b };
}

void keyboard_post_init_user(void) {
  rgb_matrix_enable();
}

const uint8_t PROGMEM ledmap[][RGB_MATRIX_LED_COUNT][3] = {
    [1] = { {43,213,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {0,0,0}, {7,255,255}, {43,213,255}, {43,213,255}, {43,213,255}, {43,213,255}, {43,213,255}, {43,213,255}, {93,255,255}, {93,255,255}, {93,255,255}, {43,213,255}, {0,0,0}, {0,0,0}, {43,213,255}, {43,213,255}, {43,213,255}, {43,213,255}, {215,213,255}, {215,213,255}, {93,255,255}, {93,255,255}, {93,255,255}, {43,213,255}, {43,213,255}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {7,255,255}, {215,213,255}, {43,213,255}, {43,213,255}, {43,213,255}, {43,213,255}, {43,213,255} },

    [2] = { {0,0,0}, {0,0,0}, {0,0,0}, {43,213,255}, {0,0,0}, {7,255,255}, {7,255,255}, {7,255,255}, {43,213,255}, {0,0,0}, {179,255,255}, {0,0,0}, {7,255,255}, {43,213,255}, {0,0,0}, {43,213,255}, {179,255,255}, {0,0,0}, {0,0,0}, {129,213,255}, {129,213,255}, {129,213,255}, {129,213,255}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {43,213,255}, {43,213,255}, {179,255,255}, {179,255,255}, {129,213,255}, {129,213,255}, {129,213,255}, {129,213,255}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {7,255,255}, {0,0,0}, {129,213,255}, {129,213,255}, {129,213,255}, {129,213,255}, {0,0,0} },

    [3] = { {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {215,213,255}, {215,213,255}, {179,255,255}, {7,255,255}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {7,255,255}, {0,0,0}, {0,0,0}, {215,213,255}, {215,213,255}, {0,0,0}, {179,255,255}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {7,255,255}, {179,255,255}, {179,255,255}, {0,0,0}, {0,0,0}, {0,0,0}, {7,255,255}, {0,0,0}, {0,0,0}, {7,255,255}, {0,0,0}, {179,255,255}, {179,255,255}, {0,0,0}, {179,255,255}, {179,255,255} },

    [4] = { {0,0,0}, {43,213,255}, {43,213,255}, {43,213,255}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {43,213,255}, {0,0,0}, {43,213,255}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {43,213,255}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {43,213,255}, {43,213,255}, {0,0,0}, {0,0,0}, {43,213,255}, {179,255,255}, {43,213,255}, {0,0,0}, {7,255,255}, {0,0,0}, {0,0,0}, {0,0,0}, {179,255,255}, {43,213,255}, {43,213,255}, {179,255,255}, {179,255,255}, {179,255,255} },

    [5] = { {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {179,255,255}, {93,255,255}, {93,255,255}, {7,255,255}, {93,255,255}, {93,255,255}, {7,255,255}, {93,255,255}, {93,255,255}, {215,213,255}, {179,255,255}, {179,255,255}, {179,255,255} },

    [6] = { {43,213,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {93,255,255}, {43,213,255}, {7,255,255}, {129,213,255}, {129,213,255}, {129,213,255}, {129,213,255}, {129,213,255}, {129,213,255}, {129,213,255}, {129,213,255}, {129,213,255}, {93,255,255}, {43,213,255}, {0,0,0}, {129,213,255}, {129,213,255}, {129,213,255}, {0,0,0}, {215,213,255}, {215,213,255}, {0,0,0}, {0,0,0}, {43,213,255}, {179,255,255}, {43,213,255}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {7,255,255}, {215,213,255}, {43,213,255}, {43,213,255}, {179,255,255}, {179,255,255}, {179,255,255} },

    [7] = { {0,0,0}, {7,255,255}, {43,213,255}, {93,255,255}, {129,213,255}, {179,255,255}, {215,213,255}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,255,255}, {43,255,255}, {86,255,255}, {129,255,255}, {172,255,255}, {215,255,255}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0} },

};

void set_layer_color(int layer) {
  for (int i = 0; i < RGB_MATRIX_LED_COUNT; i++) {
    HSV hsv = {
      .h = pgm_read_byte(&ledmap[layer][i][0]),
      .s = pgm_read_byte(&ledmap[layer][i][1]),
      .v = pgm_read_byte(&ledmap[layer][i][2]),
    };
    if (!hsv.h && !hsv.s && !hsv.v) {
        rgb_matrix_set_color( i, 0, 0, 0 );
    } else {
        RGB rgb = hsv_to_rgb_with_value(hsv);
        rgb_matrix_set_color(i, rgb.r, rgb.g, rgb.b);
    }
  }
}

bool rgb_matrix_indicators_user(void) {
  if (rawhid_state.rgb_control) {
      return false;
  }
  if (!keyboard_config.disable_layer_led) { 
    switch (biton32(layer_state)) {
      case 1:
        set_layer_color(1);
        break;
      case 2:
        set_layer_color(2);
        break;
      case 3:
        set_layer_color(3);
        break;
      case 4:
        set_layer_color(4);
        break;
      case 5:
        set_layer_color(5);
        break;
      case 6:
        set_layer_color(6);
        break;
      case 7:
        set_layer_color(7);
        break;
     default:
        if (rgb_matrix_get_flags() == LED_FLAG_NONE) {
          rgb_matrix_set_color_all(0, 0, 0);
        }
    }
  } else {
    if (rgb_matrix_get_flags() == LED_FLAG_NONE) {
      rgb_matrix_set_color_all(0, 0, 0);
    }
  }

  return true;
}




bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  switch (keycode) {

    case DUAL_FUNC_0:
      if (record->tap.count > 0) {
        if (record->event.pressed) {
          register_code16(KC_TAB);
        } else {
          unregister_code16(KC_TAB);
        }
      } else {
        if (record->event.pressed) {
          layer_on(_RAISE);
        } else {
          layer_off(_RAISE);
        }  
      }  
      return false;
    case DUAL_FUNC_1:
      if (record->tap.count > 0) {
        if (record->event.pressed) {
          layer_move(0);
        } else {
          layer_move(0);
        }
      } else {
        if (record->event.pressed) {
          layer_on(6);
        } else {
          layer_off(6);
        }  
      }  
      return false;
    case RGB_SLD:
        if (rawhid_state.rgb_control) {
            return false;
        }
        if (record->event.pressed) {
            rgblight_mode(1);
        }
        return false;
  }
  return true;
}

#ifdef AUDIO_ENABLE
bool muse_mode = false;
uint8_t last_muse_note = 0;
uint16_t muse_counter = 0;
uint8_t muse_offset = 70;
uint16_t muse_tempo = 50;

void encoder_update(bool clockwise) {
    if (muse_mode) {
        if (IS_LAYER_ON(_RAISE)) {
            if (clockwise) {
                muse_offset++;
            } else {
                muse_offset--;
            }
        } else {
            if (clockwise) {
                muse_tempo+=1;
            } else {
                muse_tempo-=1;
            }
        }
    } else {
        if (clockwise) {
        #ifdef MOUSEKEY_ENABLE
            register_code(KC_MS_WH_DOWN);
            unregister_code(KC_MS_WH_DOWN);
        #else
            register_code(KC_PGDN);
            unregister_code(KC_PGDN);
        #endif
        } else {
        #ifdef MOUSEKEY_ENABLE
            register_code(KC_MS_WH_UP);
            unregister_code(KC_MS_WH_UP);
        #else
            register_code(KC_PGUP);
            unregister_code(KC_PGUP);
        #endif
        }
    }
}

void matrix_scan_user(void) {
#ifdef AUDIO_ENABLE
    if (muse_mode) {
        if (muse_counter == 0) {
            uint8_t muse_note = muse_offset + SCALE[muse_clock_pulse()];
            if (muse_note != last_muse_note) {
                stop_note(compute_freq_for_midi_note(last_muse_note));
                play_note(compute_freq_for_midi_note(muse_note), 0xF);
                last_muse_note = muse_note;
            }
        }
        muse_counter = (muse_counter + 1) % muse_tempo;
    }
#endif
}

bool music_mask_user(uint16_t keycode) {
    switch (keycode) {
    case RAISE:
    case LOWER:
        return false;
    default:
        return true;
    }
}
#endif

uint16_t layer_state_set_user(uint16_t state) {
    return update_tri_layer_state(state, _LOWER, _RAISE, _ADJUST);
}

