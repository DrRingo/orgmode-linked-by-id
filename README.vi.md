# Orgmode-linked-by-id

**Orgmode-linked-by-id** là một extension cho Emacs Org-mode giúp bạn tạo liên kết chéo bền vững giữa các tiêu đề, hình ảnh, bảng bằng ID thay vì tên tiêu đề. Điều này giải quyết triệt để vấn đề link bị hỏng khi đổi tên header, đồng thời giúp bạn dễ dàng tham chiếu đến bất kỳ mục nào trong ghi chú, kể cả khi tài liệu lớn hoặc nhiều file.

### Tại sao bạn cần extension này?
- Khi đổi tên tiêu đề, các liên kết Org truyền thống sẽ bị hỏng. Liên kết theo ID luôn hoạt động.
- Bạn muốn liên kết nhanh đến bất kỳ mục, hình ảnh, bảng nào—trong file hoặc cả thư mục—mà không phải nhớ/cài đặt ID thủ công hay lo lắng về tên tiêu đề dễ đổi.
- Bạn cần một cách tìm kiếm, chèn liên kết nhanh, tiện lợi, đặc biệt khi hệ thống ghi chú Org của bạn ngày càng lớn.

Extension này lý tưởng cho nhà nghiên cứu, sinh viên, người làm tri thức, hoặc bất kỳ ai xây dựng hệ thống ghi chú Org-mode lớn, liên kết chéo nhiều nơi (như Zettelkasten).

## Hướng dẫn cài đặt

### Cho Doom Emacs (khuyên dùng)

1. **Thêm vào `~/.config/doom/packages.el`:**
   ```elisp
   (package! orgmode-linked-by-id
     :recipe (:host github :repo "drringo/orgmode-linked-by-id"))
   ```
2. **Chạy đồng bộ Doom và khởi động lại Emacs:**
   ```sh
   doom sync
   ```
   Sau đó khởi động lại Emacs.
3. **Require package trong `~/.config/doom/config.el`:**
   ```elisp
   (require 'orgmode-linked-by-id)
   ```
   Điều này sẽ kích hoạt các phím tắt mặc định:
   - `C-c l i` : Chèn liên kết ID trong file hiện tại
   - `C-c l f` : Chèn liên kết ID trong thư mục
   - `C-c l c` : Chèn liên kết đến hình ảnh/bảng có tên

4. **(Tuỳ chọn) Keybinding kiểu Doom (SPC l i ...):**
   Nếu muốn dùng phím tắt kiểu Doom, thêm vào `config.el`:
   ```elisp
   (when (fboundp 'map!)
     (map! :leader
           (:prefix ("l" . "org link by id")
            :desc "Insert ID in file"   "i" #'drringo/org-get-headers-with-ids
            :desc "Insert ID in folder" "f" #'drringo/org-get-headers-with-ids-in-folder
            :desc "Insert entity link"  "c" #'drringo/helm-insert-org-image-link-with-custom-caption)))
   ```
   > **Lưu ý:** Bản thân package KHÔNG dùng `map!` bên trong, nên tương thích cả Doom và Emacs thường.

### Cho Emacs thường (hoặc các bản phân phối khác)

1. **Clone repo:**
   ```sh
   git clone https://github.com/drringo/orgmode-linked-by-id.git ~/.emacs.d/lisp/orgmode-linked-by-id
   ```
2. **Thêm vào `init.el` hoặc `.emacs`:**
   ```elisp
   (add-to-list 'load-path "~/.emacs.d/lisp/orgmode-linked-by-id")
   (require 'orgmode-linked-by-id)
   ```
3. **Sử dụng các phím tắt mặc định:**
   - `C-c l i` : Chèn liên kết ID trong file hiện tại
   - `C-c l f` : Chèn liên kết ID trong thư mục
   - `C-c l c` : Chèn liên kết đến hình ảnh/bảng có tên

---

## Lý do sử dụng ID
- **Tránh vấn đề khi header thay đổi nội dung**
- **Không cần suy nghĩ về CUSTOM_ID**
- **ID được tạo tự động bằng `org-id-get-create`**

## Tính năng & Ý tưởng
- **Liên kết header trong file hiện tại**: Tìm tất cả header có ID trong file đang mở, chọn từ danh sách và chèn liên kết ID vào vị trí con trỏ.
- **Liên kết header trong thư mục**: Tìm header có ID trong tất cả file .org trong thư mục (không bao gồm thư mục con), sử dụng Helm để tìm kiếm nhanh, chọn tương tác.
- **Liên kết đến hình ảnh/bảng có tên**: Tìm đối tượng có `#+NAME` và `#+CAPTION`, hiển thị danh sách caption cho người dùng chọn, cho phép tuỳ chỉnh mô tả liên kết.

## So sánh với phương pháp truyền thống
| Phương pháp truyền thống | Extension này |
|--------------------------|---------------|
| `[[*Header name][Header name]]` | `[[id:abc123][Header name]]` |
| Hỏng khi header thay đổi | Vẫn hoạt động |
| Khó tìm kiếm | Tìm kiếm dễ dàng |

---

## Lưu ý
- **KHÔNG** dùng `map!` hoặc macro đặc biệt của Doom trong file package. Chỉ dùng trong config cá nhân nếu bạn dùng Doom.
- Package tương thích cả Doom Emacs và Emacs thường.
- Tất cả phím tắt đều có sẵn qua `global-set-key` mặc định.

---

## Gỡ cài đặt
- Xoá các dòng liên quan trong `packages.el` và `config.el` (Doom) hoặc trong `init.el` (Emacs thường).
- Chạy lại `doom sync` nếu dùng Doom.

---

## Khắc phục sự cố
- Nếu gặp lỗi về `:prefix` hoặc `map!`, hãy chắc chắn đã xoá hết các dòng `map!` trong file package, chỉ dùng trong config Doom cá nhân.
- Nếu không tìm thấy package, kiểm tra repo có public, đường dẫn đúng và đã chạy `doom sync`. 